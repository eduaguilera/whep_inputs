default_whep_inputs_path <- function() {
  fs::path("Model inputs", "world")
}

validate_input_paths <- function(input_paths, allow_dirs = TRUE) {
  input_paths <- fs::path_expand(input_paths)
  exists <- fs::file_exists(input_paths) | fs::dir_exists(input_paths)

  if (any(!exists)) {
    missing <- input_paths[!exists]
    cli::cli_abort("Input path{?s} not found: {missing}")
  }

  if (!allow_dirs && any(fs::is_dir(input_paths))) {
    cli::cli_abort("Directory inputs are not valid for this upload type.")
  }

  input_paths
}

path_ext_lower <- function(path) {
  file_name <- stringr::str_to_lower(fs::path_file(path))

  dplyr::case_when(
    stringr::str_detect(file_name, "\\.tar\\.gz$") ~ "tar.gz",
    stringr::str_detect(file_name, "\\.tar\\.bz2$") ~ "tar.bz2",
    stringr::str_detect(file_name, "\\.tar\\.xz$") ~ "tar.xz",
    TRUE ~ stringr::str_to_lower(fs::path_ext(path))
  )
}

auto_tabular_extensions <- function() {
  c("csv", "tsv", "txt", "rds", "rda", "rdata")
}

infer_upload_type <- function(input_paths) {
  input_paths <- validate_input_paths(input_paths)

  if (any(fs::is_dir(input_paths))) {
    return("archive")
  }

  if (length(input_paths) != 1L) {
    return("files")
  }

  ext <- path_ext_lower(input_paths)
  if (ext %in% auto_tabular_extensions()) "tabular" else "files"
}

read_tabular_input <- function(input_path, sheet = NULL, ...) {
  input_path <- validate_input_paths(input_path, allow_dirs = FALSE)

  if (length(input_path) != 1L) {
    cli::cli_abort("Tabular uploads require exactly one input file.")
  }

  ext <- path_ext_lower(input_path)
  data <- switch(
    ext,
    csv = readr::read_csv(input_path, show_col_types = FALSE, ...),
    tsv = readr::read_tsv(input_path, show_col_types = FALSE, ...),
    txt = readr::read_delim(input_path, delim = "\t", show_col_types = FALSE, ...),
    rds = readRDS(input_path),
    rda = read_rda_table(input_path),
    rdata = read_rda_table(input_path),
    parquet = arrow::read_parquet(input_path, ...),
    cli::cli_abort("Unsupported tabular extension: .{ext}")
  )

  if (!is.data.frame(data)) {
    cli::cli_abort("Expected a data frame, but {input_path} produced {class(data)[[1]]}.")
  }

  tibble::as_tibble(data)
}

read_rda_table <- function(input_path) {
  env <- new.env(parent = emptyenv())
  loaded <- load(input_path, envir = env)
  data_names <- loaded[purrr::map_lgl(loaded, ~ is.data.frame(env[[.x]]))]

  if (length(data_names) != 1L) {
    cli::cli_abort(
      "{input_path} must contain exactly one data frame; found {length(data_names)}."
    )
  }

  env[[data_names]]
}

copy_to_archive_stage <- function(input_path, stage_dir) {
  target_path <- fs::path(stage_dir, fs::path_file(input_path))

  if (fs::is_dir(input_path)) {
    fs::dir_copy(input_path, target_path, overwrite = TRUE)
  } else {
    fs::file_copy(input_path, target_path, overwrite = TRUE)
  }
}

archive_input_paths <- function(input_paths, pin_name, archive_name = NULL) {
  input_paths <- validate_input_paths(input_paths)

  archive_name <- archive_name %||% stringr::str_glue("{pin_name}.tar.gz")
  archive_path <- fs::path(tempdir(), archive_name)
  stage_dir <- fs::dir_create(tempfile(pattern = stringr::str_glue("{pin_name}-")))

  if (fs::file_exists(archive_path)) {
    fs::file_delete(archive_path)
  }

  purrr::walk(input_paths, copy_to_archive_stage, stage_dir = stage_dir)

  old_wd <- setwd(stage_dir)
  on.exit(setwd(old_wd), add = TRUE)

  utils::tar(
    archive_path,
    files = fs::dir_ls(".", all = TRUE, recurse = FALSE) |> fs::path_file(),
    compression = "gzip",
    tar = "tar"
  )

  cli::cli_alert_info("Archived {length(input_paths)} path{?s} into {archive_path}")
  archive_path
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

prepare_pin_paths <- function(input_paths,
                              pin_name,
                              type = c("auto", "tabular", "files", "archive"),
                              year_col = NULL,
                              sheet = NULL,
                              archive_name = NULL,
                              ...) {
  type <- match.arg(type)

  if (identical(type, "auto")) {
    type <- infer_upload_type(input_paths)
    cli::cli_alert_info("Using upload type {.val {type}} for {pin_name}")
  }

  switch(
    type,
    tabular = input_paths |>
      read_tabular_input(sheet = sheet, ...) |>
      save_processed_tibble(pin_name, year_col = year_col),
    files = validate_input_paths(input_paths),
    archive = archive_input_paths(input_paths, pin_name, archive_name = archive_name)
  )
}

upload_input <- function(input_paths,
                         pin_name,
                         remote_path = default_whep_inputs_path(),
                         type = c("auto", "tabular", "files", "archive"),
                         year_col = NULL,
                         sheet = NULL,
                         archive_name = NULL,
                         ...) {
  paths <- prepare_pin_paths(
    input_paths,
    pin_name,
    type = type,
    year_col = year_col,
    sheet = sheet,
    archive_name = archive_name,
    ...
  )

  paths |>
    prepare_for_upload(pin_name) |>
    upload_remote(remote_path) |>
    update_pins_yaml(remote_path)

  cli::cli_alert_success("Successfully uploaded {pin_name}")
  invisible(pin_name)
}

upload_tabular <- function(input_path,
                           pin_name,
                           remote_path = default_whep_inputs_path(),
                           year_col = NULL,
                           sheet = NULL,
                           ...) {
  upload_input(
    input_path,
    pin_name,
    remote_path = remote_path,
    type = "tabular",
    year_col = year_col,
    sheet = sheet,
    ...
  )
}

upload_archive <- function(input_paths,
                           pin_name,
                           remote_path = default_whep_inputs_path(),
                           archive_name = NULL) {
  upload_input(
    input_paths,
    pin_name,
    remote_path = remote_path,
    type = "archive",
    archive_name = archive_name
  )
}

prepare_for_upload <- function(input_paths, data_name, ...) {
  input_paths <- validate_input_paths(input_paths)
  tmp_board <- pins::board_temp(versioned = TRUE)

  tmp_board |>
    pins::pin_upload(input_paths, data_name, ...)

  tmp_board
}

get_all_pins <- function(pins_board) {
  pins_board |>
    pins::pin_list() |>
    purrr::map_dfr(function(pin) {
      pins_board |>
        pins::pin_versions(pin) |>
        dplyr::mutate(pin = pin, .before = 1)
    })
}

upload_remote <- function(tmp_board, remote_path) {
  root_path <- add_whep_prefix(remote_path)
  tmp_board_path <- tmp_board$path

  tmp_board_path |>
    fs::dir_ls(recurse = TRUE) |>
    purrr::map(~ upload_file(.x, tmp_board_path, root_path))

  tmp_board
}

nextcloud_path_user <- function() {
  id <- Sys.getenv("NEXTCLOUD_USER_ID")
  if (nzchar(id)) id else Sys.getenv("NEXTCLOUD_USER")
}

upload_file <- function(local_path, tmp_board_path, root_path) {
  rel_path <- fs::path_rel(local_path, start = tmp_board_path)
  remote_path <- fs::path(root_path, rel_path) |>
    utils::URLencode()

  if (fs::is_dir(local_path)) {
    maybe_create_remote_folder(remote_path)
  } else {
    httr::with_config(
      httr::progress("up"),
      kwb.nextcloud::upload_file(
        file = local_path,
        target_path = fs::path_dir(remote_path),
        user = nextcloud_path_user()
      )
    )
  }
}

maybe_create_remote_folder <- function(remote_path) {
  tryCatch(
    kwb.nextcloud::create_folder(remote_path, user = nextcloud_path_user()),
    error = function(e) {
      if (!stringr::str_detect(e$message, "already exists")) {
        cli::cli_abort("Unexpected error: {e$message}")
      }
    }
  )
}

get_remote_board <- function(raw_inputs_path) {
  pins_yaml_path <- fs::path(raw_inputs_path, "_pins.yaml")

  "NEXTCLOUD_WHEP" |>
    Sys.getenv() |>
    paste0(pins_yaml_path) |>
    utils::URLencode() |>
    pins::board_url()
}

build_updated_pins_yaml <- function(remote_pins, tmp_pins) {
  remote_pins |>
    dplyr::bind_rows(tmp_pins) |>
    dplyr::mutate(version = stringr::str_glue("{pin}/{version}/")) |>
    dplyr::arrange(pin, version) |>
    dplyr::summarise(versions = list(version), .by = c("pin")) |>
    tibble::deframe()
}

write_tmp_pins_yaml <- function(remote_board, tmp_board) {
  remote_pins <- get_all_pins(remote_board)
  tmp_pins <- get_all_pins(tmp_board)

  temp_path <- fs::path(tempdir(), "_pins.yaml")

  remote_pins |>
    build_updated_pins_yaml(tmp_pins) |>
    yaml::write_yaml(temp_path)

  temp_path
}

update_pins_yaml <- function(tmp_board, remote_path) {
  remote_board <- get_remote_board(remote_path)

  httr::with_config(
    httr::progress("up"),
    kwb.nextcloud::upload_file(
      file = write_tmp_pins_yaml(remote_board, tmp_board),
      target_path = remote_path |>
        add_whep_prefix() |>
        utils::URLencode(),
      user = nextcloud_path_user()
    )
  )
}

add_whep_prefix <- function(path) {
  fs::path("WHEP_ERC 2025", path)
}

save_processed_tibble <- function(data,
                                  name,
                                  year_col = NULL,
                                  formats = c("csv", "parquet"),
                                  out_dir = tempdir(),
                                  ...) {
  formats <- match.arg(formats, c("csv", "parquet"), several.ok = TRUE)
  fs::dir_create(out_dir)
  paths <- fs::path(out_dir, stringr::str_glue("{name}.{formats}"))
  names(paths) <- formats

  # Auto-detect year column if not provided
  if (is.null(year_col)) {
    candidates <- intersect(c("Year", "year"), colnames(data))
    year_col <- if (length(candidates) > 0) candidates[[1]] else NULL
  }

  if (is.null(year_col)) {
    cli::cli_warn("{name}: no year column found, parquet will not be sorted by year")
  } else {
    data <- dplyr::arrange(data, .data[[year_col]])
  }

  if ("csv" %in% formats) {
    readr::write_csv(data, paths[["csv"]])
  }
  if ("parquet" %in% formats) {
    arrow::write_parquet(data, paths[["parquet"]], chunk_size = 500000L)
  }

  # Verify written row count matches source
  if ("parquet" %in% formats) {
    written_rows <- arrow::read_parquet(paths[["parquet"]], col_select = 1L) |>
      nrow()
    if (written_rows != nrow(data)) {
      cli::cli_abort(
        "{name}: parquet has {written_rows} rows but source has {nrow(data)}"
      )
    }

    n_groups <- arrow::ParquetFileReader$create(paths[["parquet"]])$num_row_groups
    cli::cli_alert_info("{name}: {written_rows} rows, {n_groups} row groups")
  }

  paths
}
