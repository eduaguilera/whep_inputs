process_file <- function(file_path) {
  if (fs::is_dir(file_path)) {
    return(NULL)
  }

  df <- if (fs::path_ext(file_path) == "rds") {
    readRDS(file_path)
  } else if (fs::path_ext(file_path) == "csv") {
    readr::read_csv(file_path, show_col_types = FALSE)
  } else {
    cli::cli_abort("Unexpected file extension {fs::path_ext(file_path)}")
  }

  tibble::tibble(df)
}

process_example <- function(root_path) {
  root_path |>
    fs::dir_ls(recurse = TRUE) |>
    purrr::map(process_file) |>
    dplyr::bind_rows()
}

source("R/upload_utils.R")

# This is just an example, the input path doesn't need to be included in
# the repository. You can also just have it locally. After all, we do these
# things precisely because the files we use are too large to store in git.
example_path <- "example/ex_1"

pin_name <- "example-multiple-files"
raw_inputs_path <- fs::path("Model inputs", "raw_inputs")
whep_inputs_path <- fs::path("Model inputs", "world")

# Upload raw inputs for transparency
example_path |>
  prepare_for_upload(pin_name) |>
  upload_remote(raw_inputs_path) |>
  update_pins_yaml(raw_inputs_path)

# Upload processed version for use in whep package
example_path |>
  process_example() |>
  save_processed_tibble(pin_name) |>
  prepare_for_upload(pin_name) |>
  upload_remote(whep_inputs_path) |>
  update_pins_yaml(whep_inputs_path)
