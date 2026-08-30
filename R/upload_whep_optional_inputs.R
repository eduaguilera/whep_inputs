source("R/upload_utils.R")

default_lpjml_grass_run_dir <- function() {
  env_path <- Sys.getenv("WHEP_LPJML_RUN_DIR", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }

  # The 1901-2009 run this used to name was deleted in the 2026-07 cleanup; the
  # current run is the 1901-2023 one. Kept as a convenience fallback only --
  # WHEP_LPJML_RUN_DIR above is the supported way to point at a run.
  local_path <- paste0(
    "/home/usuario/Nextcloud/WHEP_ERC 2025/Sources/datasets/",
    "unclassified_datasets/LPJmL/LPJmL_runs/",
    "global_1901-2023_spinup_200_our_inputs"
  )

  if (fs::dir_exists(local_path)) local_path else NA_character_
}

whep_spatialize_pin_map <- function() {
  tibble::tribble(
    ~file_name, ~pin_name, ~required,
    "country_areas.parquet", "spatialize-country-areas", TRUE,
    "crop_patterns.parquet", "spatialize-crop-patterns", TRUE,
    "gridded_cropland.parquet", "spatialize-gridded-cropland", TRUE,
    "country_grid.parquet", "spatialize-country-grid", TRUE,
    "type_cropland.parquet", "spatialize-type-cropland", FALSE,
    "multicropping.parquet", "spatialize-multicropping", FALSE,
    "livestock_country_data.parquet", "spatialize-livestock-country-data", TRUE,
    "gridded_pasture.parquet", "spatialize-gridded-pasture", TRUE,
    "manure_pattern.parquet", "spatialize-manure-pattern", FALSE
  )
}

check_whep_spatialize_inputs <- function(
  input_dir = "~/WHEP/LPJmL_inputs/whep/inputs",
  include_optional = TRUE
) {
  input_dir <- fs::path_expand(input_dir)
  pin_map <- whep_spatialize_pin_map()

  if (!include_optional) {
    pin_map <- dplyr::filter(pin_map, required)
  }

  pin_map <- dplyr::mutate(
    pin_map,
    local_path = fs::path(input_dir, file_name),
    exists = fs::file_exists(local_path)
  )

  missing_required <- dplyr::filter(pin_map, required, !exists)
  if (nrow(missing_required) > 0) {
    cli::cli_abort(c(
      "Missing required WHEP spatialize input{?s}:",
      "x" = "{missing_required$file_name}"
    ))
  }

  missing_optional <- dplyr::filter(pin_map, !required, !exists)
  if (nrow(missing_optional) > 0) {
    cli::cli_warn(c(
      "Skipping optional WHEP spatialize input{?s}:",
      "i" = "{missing_optional$file_name}"
    ))
  }

  dplyr::filter(pin_map, exists)
}

upload_whep_spatialize_inputs <- function(
  input_dir = "~/WHEP/LPJmL_inputs/whep/inputs",
  remote_path = default_whep_inputs_path(),
  include_optional = TRUE
) {
  pin_map <- check_whep_spatialize_inputs(
    input_dir = input_dir,
    include_optional = include_optional
  )

  purrr::pwalk(
    dplyr::select(pin_map, local_path, pin_name),
    function(local_path, pin_name) {
      upload_input(local_path, pin_name, remote_path = remote_path, type = "files")
    }
  )

  invisible(pin_map)
}

upload_whep_lpjml_grass_artifacts <- function(
  availability_path,
  productivity_path,
  remote_path = default_whep_inputs_path(),
  type = c("auto", "tabular", "files")
) {
  type <- match.arg(type)

  paths <- c(
    "lpjml-grass-availability" = availability_path,
    "lpjml-grass-productivity" = productivity_path
  )

  purrr::iwalk(paths, function(local_path, pin_name) {
    upload_input(local_path, pin_name, remote_path = remote_path, type = type)
  })

  invisible(paths)
}

upload_whep_lpjml_grass_from_run <- function(
  run_dir = default_lpjml_grass_run_dir(),
  remote_path = default_whep_inputs_path(),
  years = NULL,
  first_year = NULL,
  shares = lpjml_grass_access_shares(),
  artifact_dir = fs::path(tempdir(), "whep_lpjml_grass_artifacts")
) {
  paths <- prepare_whep_lpjml_grass_artifacts(
    run_dir = run_dir,
    years = years,
    first_year = first_year,
    shares = shares,
    artifact_dir = artifact_dir
  )

  purrr::iwalk(paths, function(local_path, pin_name) {
    upload_input(local_path, pin_name, remote_path = remote_path, type = "files")
  })

  invisible(paths)
}

# The calendar year of a run's first output step, decoded from the file's own
# time axis instead of assumed.
#
# Every LPJmL output stamps its time axis as "days since YYYY-M-D" on a noleap
# calendar. This used to default to 1901 in five places here, which silently
# relabels every year of the 1750-2023 run by 151 -- with no error, and no
# symptom in any pin, because the artifact still has a valid schema.
#
# Mirrors whep's own .lpjml_first_year(); duplicated here rather than
# reaching into the package, because the grass readers below do their own
# NetCDF arithmetic and must resolve the year before whep is loaded.
lpjml_run_first_year <- function(run_dir, file = NULL) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  candidates <- c(file, "pft_npp.nc", "mswc.nc", "cftfrac.nc", "fpc.nc")
  path <- NULL
  for (f in candidates) {
    if (!is.null(f) && fs::file_exists(fs::path(run_dir, f))) {
      path <- fs::path(run_dir, f)
      break
    }
  }
  if (is.null(path)) {
    cli::cli_abort("No LPJmL output in {.path {run_dir}} to read a year from.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  units <- nc$dim[["time"]]$units %||% ""
  # Parsed without regex escapes on purpose: this string has travelled
  # through several layers of quoting and a mangled backslash here would
  # fail open, not closed.
  after <- sub("^.*since[ ]*", "", units)
  ref <- sub("-.*$", "", after)
  vals <- nc$dim[["time"]]$vals
  if (!grepl("^[0-9]{3,4}$", ref)) {
    cli::cli_abort(c(
      "Cannot tell which year {.file {path}} starts in.",
      i = "Its time axis carries no {.val since YYYY-} reference.",
      i = "Pass {.arg first_year} explicitly."
    ))
  }
  as.integer(ref) + as.integer(floor(vals[[1]] / 365))
}

prepare_whep_lpjml_grass_artifacts <- function(
  run_dir = default_lpjml_grass_run_dir(),
  years = NULL,
  first_year = NULL,
  shares = lpjml_grass_access_shares(),
  artifact_dir = fs::path(tempdir(), "whep_lpjml_grass_artifacts")
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  first_year <- first_year %||% lpjml_run_first_year(run_dir)
  fs::dir_create(artifact_dir)

  cli::cli_alert_info("Preparing LPJmL grass availability from {.path {run_dir}}")
  availability <- read_lpjml_managed_grass_availability(
    run_dir = run_dir,
    years = years,
    first_year = first_year,
    shares = shares
  )
  availability_path <- save_processed_tibble(
    availability,
    "lpjml-grass-availability",
    year_col = "year",
    formats = "parquet",
    out_dir = artifact_dir
  )[[1]] |>
    unname()

  cli::cli_alert_info("Preparing LPJmL grass productivity from {.path {run_dir}}")
  productivity <- read_lpjml_natural_grass_productivity(
    run_dir = run_dir,
    years = years,
    first_year = first_year
  )
  productivity_path <- save_processed_tibble(
    productivity,
    "lpjml-grass-productivity",
    year_col = "year",
    formats = "parquet",
    out_dir = artifact_dir
  )[[1]] |>
    unname()

  c(
    "lpjml-grass-availability" = availability_path,
    "lpjml-grass-productivity" = productivity_path
  )
}

#' Upload the two LPJmL-derived SOC/nitrogen artifacts.
#'
#' These exist so a WHEP user never has to run LPJmL to get the historical
#' carbon balance and the gridded nitrogen balance that depends on it. Both
#' carry ONLY LPJmL-derived quantities: the grazing excreta, the humification
#' fractions, CRU air temperature and the HWSD texture products are all
#' computed locally by the package, so pinning these two layers freezes nothing
#' a user could otherwise choose or download.
#'
#' Generate the files with the whep-side generator (they need the package's
#' readers), then pass their paths here.
upload_whep_lpjml_soc_artifacts <- function(
  net_c_path,
  hydrology_path,
  remote_path = default_whep_inputs_path(),
  type = c("files", "auto", "tabular")
) {
  type <- match.arg(type)

  paths <- c(
    "lpjml-grass-natural-net-c" = net_c_path,
    "lpjml-soc-hydrology" = hydrology_path
  )

  validate_input_paths(unname(paths), allow_dirs = FALSE)

  purrr::iwalk(paths, function(local_path, pin_name) {
    cli::cli_alert_info(
      "Uploading {.val {pin_name}} ({round(fs::file_size(local_path) / 1024^2)} MB)"
    )
    upload_input(local_path, pin_name, remote_path = remote_path, type = type)
  })

  invisible(paths)
}

resolve_lpjml_output_dir <- function(run_dir) {
  if (is.null(run_dir) || is.na(run_dir) || !nzchar(run_dir)) {
    cli::cli_abort(c(
      "No LPJmL run directory was supplied.",
      i = "Set {.envvar WHEP_LPJML_RUN_DIR} or pass {.arg run_dir} explicitly."
    ))
  }

  run_dir <- fs::path_expand(run_dir)
  candidates <- c(run_dir, fs::path(run_dir, "output", "scenario_1"))
  has_outputs <- fs::file_exists(fs::path(candidates, "pft_npp.nc")) &
    fs::file_exists(fs::path(candidates, "cftfrac.nc"))

  if (!any(has_outputs)) {
    cli::cli_abort(c(
      "Could not find LPJmL grass output files under {.path {run_dir}}.",
      i = "Expected {.file pft_npp.nc} and {.file cftfrac.nc} either directly",
      i = "or under {.file output/scenario_1}."
    ))
  }

  candidates[which(has_outputs)[[1]]]
}

lpjml_grass_access_shares <- function(
  aboveground = 0.46,
  grazable = 1,
  w_c_dm = 0.45
) {
  list(aboveground = aboveground, grazable = grazable, w_c_dm = w_c_dm)
}

read_lpjml_managed_grass_availability <- function(
  run_dir = default_lpjml_grass_run_dir(),
  years = NULL,
  first_year = NULL,
  shares = lpjml_grass_access_shares()
) {
  require_lpjml_reader_packages()
  run_dir <- resolve_lpjml_output_dir(run_dir)
  first_year <- first_year %||% lpjml_run_first_year(run_dir, "pft_npp.nc")

  npp <- ncdf4::nc_open(fs::path(run_dir, "pft_npp.nc"))
  frac <- ncdf4::nc_open(fs::path(run_dir, "cftfrac.nc"))
  on.exit(ncdf4::nc_close(npp), add = TRUE)
  on.exit(ncdf4::nc_close(frac), add = TRUE)

  lon <- ncdf4::ncvar_get(npp, "lon")
  lat <- ncdf4::ncvar_get(npp, "lat")
  assert_same_grid(lon, lat, frac, fs::path(run_dir, "cftfrac.nc"))

  bands <- c("rainfed grassland", "irrigated grassland")
  npp_idx <- lpjml_band_index(npp, bands, fs::path(run_dir, "pft_npp.nc"))
  frac_idx <- lpjml_band_index(frac, bands, fs::path(run_dir, "cftfrac.nc"))
  years <- clip_lpjml_years(
    years,
    first_year,
    min(npp$dim[["time"]]$len, frac$dim[["time"]]$len),
    fs::path(run_dir, "pft_npp.nc")
  )

  grid <- lpjml_grid(lon, lat)
  rows <- vector("list", length(years))
  cell_area_ha <- cell_area_ha_lat(grid$lat)

  for (i in seq_along(years)) {
    time_idx <- years[[i]] - first_year + 1L
    grass_npp <- numeric(nrow(grid))

    for (band in seq_along(bands)) {
      npp_slab <- lpjml_slab(npp, "NPP", npp_idx[[band]], time_idx, lon, lat)
      frac_slab <- lpjml_slab(
        frac,
        "CFTfrac",
        frac_idx[[band]],
        time_idx,
        lon,
        lat
      )
      grass_npp <- grass_npp + zero_invalid(npp_slab) * zero_invalid(frac_slab)
    }

    keep <- grass_npp > 0
    grass_avail_dm_t_ha <- lpjml_grass_to_dm(grass_npp[keep], shares)
    rows[[i]] <- tibble::tibble(
      lon = grid$lon[keep],
      lat = grid$lat[keep],
      year = as.integer(years[[i]]),
      grass_npp_gc_m2 = grass_npp[keep],
      grass_avail_dm_t_ha = grass_avail_dm_t_ha,
      grass_avail_dm_t = grass_avail_dm_t_ha * cell_area_ha[keep]
    )
  }

  dplyr::bind_rows(rows)
}

read_lpjml_natural_grass_productivity <- function(
  run_dir = default_lpjml_grass_run_dir(),
  years = NULL,
  first_year = NULL
) {
  require_lpjml_reader_packages()
  run_dir <- resolve_lpjml_output_dir(run_dir)
  first_year <- first_year %||% lpjml_run_first_year(run_dir, "pft_npp.nc")

  npp <- ncdf4::nc_open(fs::path(run_dir, "pft_npp.nc"))
  on.exit(ncdf4::nc_close(npp), add = TRUE)

  lon <- ncdf4::ncvar_get(npp, "lon")
  lat <- ncdf4::ncvar_get(npp, "lat")
  bands <- c("Tropical C4 grass", "Temperate C3 grass", "Polar C3 grass")
  band_idx <- lpjml_band_index(npp, bands, fs::path(run_dir, "pft_npp.nc"))
  years <- clip_lpjml_years(
    years,
    first_year,
    npp$dim[["time"]]$len,
    fs::path(run_dir, "pft_npp.nc")
  )

  grid <- lpjml_grid(lon, lat)
  rows <- vector("list", length(years))

  for (i in seq_along(years)) {
    time_idx <- years[[i]] - first_year + 1L
    grass_npp <- numeric(nrow(grid))

    for (band in seq_along(bands)) {
      grass_npp <- grass_npp +
        zero_invalid(lpjml_slab(npp, "NPP", band_idx[[band]], time_idx, lon, lat))
    }

    keep <- grass_npp > 0
    rows[[i]] <- tibble::tibble(
      lon = grid$lon[keep],
      lat = grid$lat[keep],
      year = as.integer(years[[i]]),
      grass_npp = grass_npp[keep]
    )
  }

  dplyr::bind_rows(rows)
}

require_lpjml_reader_packages <- function() {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ncdf4} is required to read LPJmL NetCDF outputs."
    )
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg arrow} is required to write LPJmL Parquet artifacts."
    )
  }
}

lpjml_band_index <- function(nc, band_names, path) {
  pft <- as.character(ncdf4::ncvar_get(nc, "NamePFT"))
  band_idx <- match(band_names, pft)

  if (anyNA(band_idx)) {
    cli::cli_abort(
      "Band{?s} not in {.file {path}}: {band_names[is.na(band_idx)]}."
    )
  }

  band_idx
}

clip_lpjml_years <- function(years, first_year, n_time, path) {
  available <- first_year + seq_len(n_time) - 1L
  if (is.null(years)) {
    return(available)
  }

  dropped <- setdiff(years, available)
  if (length(dropped) > 0) {
    cli::cli_warn(c(
      "{length(dropped)} requested year{?s} fall outside the run coverage",
      "i" = "{min(available)}-{max(available)} in {.file {basename(path)}}."
    ))
  }

  years <- intersect(as.integer(years), available)
  if (length(years) == 0L) {
    cli::cli_abort("No requested years overlap the LPJmL run coverage.")
  }

  years
}

lpjml_grid <- function(lon_values, lat_values) {
  n_lon <- length(lon_values)
  tibble::tibble(
    lon = rep(lon_values, times = length(lat_values)),
    lat = rep(lat_values, each = n_lon)
  )
}

assert_same_grid <- function(lon, lat, nc, path) {
  other_lon <- ncdf4::ncvar_get(nc, "lon")
  other_lat <- ncdf4::ncvar_get(nc, "lat")

  if (!identical(lon, other_lon) || !identical(lat, other_lat)) {
    cli::cli_abort("Grid mismatch in {.file {path}}.")
  }
}

lpjml_slab <- function(nc, var_name, band_idx, time_idx, lon, lat) {
  ncdf4::ncvar_get(
    nc,
    var_name,
    start = c(1L, 1L, band_idx, time_idx),
    count = c(length(lon), length(lat), 1L, 1L)
  ) |>
    as.vector()
}

zero_invalid <- function(values) {
  values[!is.finite(values) | values <= -1e30] <- 0
  values
}

lpjml_grass_to_dm <- function(grass_npp_gc_m2, shares) {
  grass_npp_gc_m2 * shares$aboveground * shares$grazable * 0.01 / shares$w_c_dm
}

cell_area_ha_lat <- function(lat) {
  earth_radius_m <- 6371000
  half_step_rad <- 0.25 * pi / 180
  lon_step_rad <- 0.5 * pi / 180
  band <- sin(lat * pi / 180 + half_step_rad) -
    sin(lat * pi / 180 - half_step_rad)
  earth_radius_m^2 * lon_step_rad * band / 1e4
}

if (FALSE) {
  upload_whep_spatialize_inputs("~/WHEP/LPJmL_inputs/whep/inputs")

  upload_whep_lpjml_grass_from_run()

  upload_whep_lpjml_grass_artifacts(
    availability_path = "~/path/to/lpjml_grass_availability.parquet",
    productivity_path = "~/path/to/lpjml_grass_productivity.parquet"
  )
}
