prepare_for_upload <- function(input_paths, data_name, ...) {
  tmp_board <- pins::board_temp(versioned = TRUE)

  tmp_board |>
    pins::pin_upload(input_paths, data_name)

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

upload_file <- function(local_path, tmp_board_path, root_path) {
  rel_path <- stringr::str_remove(local_path, tmp_board_path)
  remote_path <- fs::path(root_path, rel_path) |>
    utils::URLencode()

  if (fs::is_dir(local_path)) {
    maybe_create_remote_folder(remote_path)
  } else {
    httr::with_config(
      httr::progress("up"),
      kwb.nextcloud::upload_file(
        file = local_path,
        target_path = fs::path_dir(remote_path)
      )
    )
  }
}

maybe_create_remote_folder <- function(remote_path) {
  tryCatch(kwb.nextcloud::create_folder(remote_path), error = function(e) {
    if (!stringr::str_detect(e$message, "already exists")) {
      cli::cli_abort("Unexpected error: {e$message}")
    }
  })
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
        utils::URLencode()
    )
  )
}

add_whep_prefix <- function(path) {
  fs::path("WHEP_ERC 2025", path)
}

save_processed_tibble <- function(data, name, year_col = NULL, ...) {
  tmp_dir <- tempdir()
  paths <- file.path(
    tmp_dir,
    c(
      stringr::str_glue("{name}.csv"),
      stringr::str_glue("{name}.parquet")
    )
  )

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

  readr::write_csv(data, paths[[1]])
  arrow::write_parquet(data, paths[[2]], chunk_size = 500000L)

  # Verify written row count matches source
  written_rows <- arrow::read_parquet(paths[[2]], col_select = 1L) |> nrow()
  if (written_rows != nrow(data)) {
    cli::cli_abort("{name}: parquet has {written_rows} rows but source has {nrow(data)}")
  }

  n_groups <- arrow::ParquetFileReader$create(paths[[2]])$num_row_groups
  cli::cli_alert_info("{name}: {written_rows} rows, {n_groups} row groups")

  paths
}
