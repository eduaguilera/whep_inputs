source("R/upload_utils.R")

upload_files <- function(file_paths,
                         pin_name,
                         remote_path = default_whep_inputs_path()) {
  upload_input(file_paths, pin_name, remote_path = remote_path, type = "files")
}

# ---------------------------------------------------------------------------- #
# INSTRUCTIONS:
# Set pin_name and file_paths, then run upload_files() or upload_archive().
# ---------------------------------------------------------------------------- #

if (FALSE) {
  # Upload prepared files as-is, for example GeoTIFF, NetCDF, GeoPackage,
  # shapefile sidecars, already-prepared Parquet files, or existing archives.
  upload_files("manure_westetal2014.tar.gz", "manure-west-et-al-2014")

  # Bundle a directory into one tar.gz pin before upload.
  upload_archive(
    "/home/usuario/OneDrive/L_files/Manure_Westetal2014",
    "manure-west-et-al-2014"
  )
}
