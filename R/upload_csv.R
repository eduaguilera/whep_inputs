source("R/upload_utils.R")

upload_csv <- function(csv_path, pin_name, whep_inputs_path = fs::path("Model inputs", "world")) {
  if (!fs::file_exists(csv_path)) {
    cli::cli_abort("File not found: {csv_path}")
  }

  if (fs::path_ext(csv_path) != "csv") {
    cli::cli_abort("File must have a .csv extension: {csv_path}")
  }

  csv_path |>
    readr::read_csv(show_col_types = FALSE) |>
    tibble::tibble() |>
    save_processed_tibble(pin_name) |>
    prepare_for_upload(pin_name) |>
    upload_remote(whep_inputs_path) |>
    update_pins_yaml(whep_inputs_path)

  cli::cli_alert_success("Successfully processed and uploaded {pin_name}")
}

# ---------------------------------------------------------------------------- #
# INSTRUCTIONS:
# Edit the variables below to point to your local CSV file, then run this script.
# The input path does not need to be within the repository.
# ---------------------------------------------------------------------------- #

example_csv_path <- "~/Downloads/Land.csv"
example_pin_name <- "land"
target_remote_path <- fs::path("Model inputs", "world")

upload_csv(example_csv_path, example_pin_name, target_remote_path)
