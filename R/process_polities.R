source("R/upload_utils.R")

# This is just an example, the input path doesn't need to be included in
# the repository. You can also just have it locally. After all, we do these
# things precisely because the files we use are too large to store in git.
example_path <- "some/path/to/whep-polities.geojson"

pin_name <- "whep_polities"
whep_inputs_path <- fs::path("Model inputs", "world")

# Upload processed version for use in whep package
example_path |>
  prepare_for_upload(pin_name) |>
  upload_remote(whep_inputs_path) |>
  update_pins_yaml(whep_inputs_path)
