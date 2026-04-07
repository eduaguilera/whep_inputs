source("R/upload_utils.R")

upload_csv <- function(csv_path, pin_name, whep_inputs_path = fs::path("Model inputs", "world"), year_col = NULL) {
  if (!fs::file_exists(csv_path)) {
    cli::cli_abort("File not found: {csv_path}")
  }

  if (fs::path_ext(csv_path) != "csv") {
    cli::cli_abort("File must have a .csv extension: {csv_path}")
  }

  csv_path |>
    readr::read_csv(show_col_types = FALSE) |>
    tibble::tibble() |>
    save_processed_tibble(pin_name, year_col = year_col) |>
    prepare_for_upload(pin_name) |>
    upload_remote(whep_inputs_path) |>
    update_pins_yaml(whep_inputs_path)

  cli::cli_alert_success("Successfully processed and uploaded {pin_name}")
}

# ---------------------------------------------------------------------------- #
# INSTRUCTIONS:
# Uncomment and run one upload_csv() call at a time.
# ---------------------------------------------------------------------------- #

remote_path <- fs::path("Model inputs", "world")

upload_csv("~/Downloads/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv", "faostat-trade-bilateral", remote_path)
