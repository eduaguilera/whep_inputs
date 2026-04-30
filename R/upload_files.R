source("R/upload_utils.R")

upload_files <- function(file_paths, pin_name, remote_path = fs::path("Model inputs", "world")) {
  missing <- file_paths[!fs::file_exists(file_paths)]
  if (length(missing) > 0) {
    cli::cli_abort("Files not found: {missing}")
  }

  file_paths |>
    prepare_for_upload(pin_name) |>
    upload_remote(remote_path) |>
    update_pins_yaml(remote_path)

  cli::cli_alert_success("Successfully uploaded {pin_name}")
  invisible(NULL)
}

# ---------------------------------------------------------------------------- #
# INSTRUCTIONS:
# Set pin_name and file_paths, then run upload_files().
# ---------------------------------------------------------------------------- #

# Example: bundle a directory and upload it as a single pin
tar("manure_westetal2014.tar.gz",
    files = "/home/usuario/OneDrive/L_files/Manure_Westetal2014",
    compression = "gzip",
    tar = "tar")
upload_files("manure_westetal2014.tar.gz", "manure-west-et-al-2014")
