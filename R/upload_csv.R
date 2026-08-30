source("R/upload_utils.R")

upload_csv <- function(csv_path,
                       pin_name,
                       whep_inputs_path = default_whep_inputs_path(),
                       year_col = NULL) {
  if (!fs::file_exists(csv_path)) {
    cli::cli_abort("File not found: {csv_path}")
  }

  if (stringr::str_to_lower(fs::path_ext(csv_path)) != "csv") {
    cli::cli_abort("File must have a .csv extension: {csv_path}")
  }

  upload_tabular(csv_path, pin_name, remote_path = whep_inputs_path, year_col = year_col)
}

# ---------------------------------------------------------------------------- #
# INSTRUCTIONS:
# Run one upload_csv() call at a time.
# ---------------------------------------------------------------------------- #

if (FALSE) {
  remote_path <- fs::path("Model inputs", "world")

  upload_csv(
    "~/Downloads/Inputs_LandUse_E_All_Data_(Normalized).csv",
    "faostat-landuse",
    remote_path
  )
  upload_csv("~/WHEP/inst/extdata/cropgrids_land.csv", "cropgrids-land", remote_path)
  upload_csv(
    "~/WHEP/inst/extdata/cropgrids_fallow_land.csv",
    "cropgrids-fallow-land",
    remote_path
  )
}
