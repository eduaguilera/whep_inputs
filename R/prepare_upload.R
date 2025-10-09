# Small script useful to setup a version folder for your data.
# Remember doing the manual steps. See prepare_for_upload call at the end
# of the script, fill with your data, run and follow printed instructions.

create_version <- function(data, board, name, ...) {
  paths <- file.path(
    tempdir(),
    c(
      stringr::str_glue("{name}.csv"),
      stringr::str_glue("{name}.parquet")
    )
  )

  readr::write_csv(data, paths[[1]])
  nanoparquet::write_parquet(data, paths[[2]])

  board |>
    pins::pin_upload(paths, name, ...)

  board |>
    pins::pin_versions(name) |>
    tail(1) |>
    dplyr::pull(version)
}

# Change this accordingly if your data is not CSV.
# Please make the output a tibble.
read_input <- function(path) {
  path |>
    readr::read_csv(show_col_types = FALSE)
}

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
      print(pin)
      pins_board |>
        pins::pin_versions(pin) |>
        dplyr::mutate(pin = pin, .before = 1)
    })
}

upload_remote <- function(tmp_board, root_path) {
  tmp_board_path <- tmp_board$path

  tmp_board_path |>
    fs::dir_ls(recurse = TRUE) |>
    purrr::map(~ upload_file(.x, tmp_board_path, root_path))
}

upload_file <- function(local_path, tmp_board_path, root_path) {
  rel_path <- stringr::str_remove(local_path, tmp_board_path)
  remote_path <- fs::path(root_path, rel_path) |>
    utils::URLencode()

  if (fs::is_dir(local_path)) {
    tryCatch(kwb.nextcloud::create_folder(remote_path), error = function(e) {
      if (!stringr::str_detect(e$message, "already exists")) {
        cli::cli_abort("Unexpected error: {e$message}")
      }
    })
  } else {
    browser()
    kwb.nextcloud::upload_file(file = local_path, target_path = remote_path)
  }
}

update_pins_yaml <- function(tmp_board, remote_board) {
  remote_pins <- get_all_pins(remote_board)
  tmp_pins <- get_all_pins(tmp_board)

  new_pins_yaml <- dplyr::bind_rows(remote_pins, tmp_pins) |>
    dplyr::arrange(pin, version)
}

k_nextcloud_url <- Sys.getenv("NEXTCLOUD_URL")
k_nextcloud_user <- Sys.getenv("NEXTCLOUD_USER")
k_nextcloud_password <- Sys.getenv("NEXTCLOUD_PASSWORD")
k_nextcloud_whep <- Sys.getenv("NEXTCLOUD_WHEP")

whep_path <- fs::path("WHEP_ERC 2025")
raw_inputs_path <- fs::path("Model inputs", "raw_inputs")
root_path <- fs::path(whep_path, raw_inputs_path)
pins_yaml_path <- fs::path(raw_inputs_path, "_pins.yaml")

pins_board <- k_nextcloud_whep |>
  paste0(pins_yaml_path) |>
  utils::URLencode() |>
  pins::board_url()

"./renv.lock" |>
  prepare_for_upload("example-file") |>
  upload_remote(root_path)
