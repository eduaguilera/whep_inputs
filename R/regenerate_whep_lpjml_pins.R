# One entry point for every WHEP input pin that is derived from LPJmL model
# OUTPUT. Call regenerate_whep_lpjml_pins() after a new LPJmL run; nothing else
# needs calling, and no other pin needs touching.
#
# WHY ONE ENTRY POINT
#
# There are seven such pins, produced by five generators that
# used to be invoked separately:
#
#   lpjml-grass-availability     read by R/feed_lpjml.R
#   lpjml-grass-productivity     read by R/feed_lpjml.R
#   lpjml-grass-natural-net-c    read by R/grass_natural_carbon_inputs.R
#   lpjml-soc-hydrology          read by R/water_balance.R
#   lpjml-crop-irrig-depth       read by R/lpjml_irrig_depth.R
#   lpjml-seepage                read by R/lpjml_hydrology.R
#   lpjml-crop-regime-yield      read by R/lpjml_regime_yield.R (the rainfed
#                                and irrigated crop yields behind WHEP's
#                                regime split of the nitrogen balance)
#
# They must be regenerated TOGETHER, from ONE run. Each one carries the same
# model's carbon and water, so refreshing a subset leaves WHEP mixing two LPJmL
# versions across its feed, soil-carbon and water chains at once -- which is
# worse than consistently using either version, and invisible downstream because
# every pin still loads and still has the right schema. Regenerating them
# separately is the failure this function exists to prevent.
#
# WHICH lpjml-* PINS THIS DELIBERATELY DOES NOT TOUCH
#
# Six others share the `lpjml-` prefix and must be left alone:
#
#   lpjml-wind-isimip-1901-2019   lpjml-wind-era5-2017-2023
#   lpjml-rsds-isimip-1901-2019   lpjml-rsds-era5-2017-2023
#   lpjml-rlds-isimip-1901-2019   lpjml-rlds-era5-2017-2023
#
# Those are climate FORCING -- they feed into LPJmL rather than coming out of
# it, so they do not change when the model version changes. They are rebuilt by
# WHEP's inst/scripts/prepare_spatialize_all.R, not here.
#
# UPLOAD IS OPT-IN
#
# Default is generate-and-report only. These seven pins are the default source
# for every WHEP user who never runs LPJmL, so replacing them changes core
# carbon and water inputs for everyone and is not easily reversed. Inspect the
# manifest and the comparison first, then re-call with `upload = TRUE`.

#' Regenerate (and optionally upload) every LPJmL-output-derived WHEP pin.
#'
#' @param run_dir A finished LPJmL run's output directory, or the run root.
#'   Defaults to `WHEP_LPJML_RUN_DIR`.
#' @param whep_path Path to the WHEP package source. The soil-carbon and
#'   hydrology artifacts are built with WHEP's own readers, so their definition
#'   lives in exactly one place rather than being restated here.
#' @param artifact_dir Where to write the seven parquet files.
#' @param years Optional year subset. `NULL` (default) does the whole run, which
#'   is what a pin needs -- pass a subset only for a smoke test.
#' @param compare If `TRUE`, report how each artifact differs from the pin it
#'   would replace. This is the point of the dry run, so it defaults on.
#' @param upload If `TRUE`, upload all seven. Defaults to `FALSE`.
#' @return A named character vector of the seven local paths, invisibly.
regenerate_whep_lpjml_pins <- function(
  run_dir = default_lpjml_grass_run_dir(),
  whep_path = whep_source_path(),
  artifact_dir = fs::path(tempdir(), "whep_lpjml_pins"),
  years = NULL,
  first_year = NULL,
  shares = lpjml_grass_access_shares(),
  compare = TRUE,
  upload = FALSE,
  remote_path = default_whep_inputs_path()
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  cohort <- lpjml_pin_cohort(run_dir, years, first_year, whep_path)
  years <- cohort$years
  first_year <- cohort$first_year
  fs::dir_create(artifact_dir)
  cli::cli_h1("Regenerating LPJmL-derived WHEP pins")
  cli::cli_alert_info("Run: {.path {run_dir}}")

  grass <- prepare_whep_lpjml_grass_artifacts(
    run_dir = run_dir,
    years = years,
    first_year = first_year,
    shares = shares,
    artifact_dir = artifact_dir
  )
  soc <- prepare_whep_lpjml_soc_artifacts(
    run_dir = run_dir,
    whep_path = whep_path,
    years = years,
    artifact_dir = artifact_dir
  )
  regime <- prepare_whep_lpjml_regime_artifacts(
    run_dir = run_dir,
    whep_path = whep_path,
    years = years,
    artifact_dir = artifact_dir
  )
  irrig <- prepare_whep_lpjml_irrig_depth_artifacts(
    run_dir = run_dir,
    whep_path = whep_path,
    years = years,
    artifact_dir = artifact_dir
  )
  seepage <- prepare_whep_lpjml_seepage_artifacts(
    run_dir = run_dir,
    whep_path = whep_path,
    years = years,
    artifact_dir = artifact_dir
  )
  paths <- c(grass, soc, regime, irrig, seepage)
  manifest <- validate_lpjml_pin_family(paths, cohort)
  readr::write_csv(
    manifest,
    fs::path(artifact_dir, "lpjml-family-manifest.csv")
  )
  attr(paths, "lpjml_manifest") <- manifest

  report_lpjml_pin_manifest(paths)
  if (isTRUE(compare)) {
    report_lpjml_pin_comparison(paths, whep_path)
  }

  if (!isTRUE(upload)) {
    cli::cli_alert_warning(
      "Nothing uploaded. Re-call with {.code upload = TRUE} to publish."
    )
    return(invisible(paths))
  }

  cli::cli_h2("Uploading")
  purrr::iwalk(paths, function(local_path, pin_name) {
    cli::cli_alert_info(
      "{.val {pin_name}} ({round(fs::file_size(local_path) / 1024^2)} MB)"
    )
    upload_input(
      local_path,
      pin_name,
      remote_path = remote_path,
      type = "files"
    )
  })
  cli::cli_alert_success(
    "Uploaded {length(paths)} pins. Update their versions in
     WHEP's inst/extdata/whep_inputs.csv, then re-run
     {.code Rscript data-raw/whep_inputs.R}."
  )

  invisible(paths)
}

#' Build the two soil-carbon/hydrology artifacts from a finished run.
#'
#' Mirrors [prepare_whep_lpjml_grass_artifacts()]. Both layers are produced by
#' WHEP's own readers rather than reimplemented here: the net-carbon layer comes
#' from the package's documented pin seam, so the pinned and run-derived paths
#' cannot drift apart.
prepare_whep_lpjml_soc_artifacts <- function(
  run_dir = default_lpjml_grass_run_dir(),
  whep_path = whep_source_path(),
  years = NULL,
  artifact_dir = fs::path(tempdir(), "whep_lpjml_pins")
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  fs::dir_create(artifact_dir)
  whep <- load_whep_namespace(whep_path)

  cli::cli_alert_info("Preparing LPJmL grass/natural net carbon")
  net_c <- build_lpjml_net_carbon(whep, run_dir, years)
  net_c_path <- unname(save_processed_tibble(
    net_c,
    "lpjml-grass-natural-net-c",
    year_col = "year",
    formats = "parquet",
    out_dir = artifact_dir
  )[[1]])

  cli::cli_alert_info("Preparing LPJmL SOC hydrology (three monthly variables)")
  hydrology <- build_lpjml_soc_hydrology(whep, run_dir, years)
  hydrology_path <- unname(save_processed_tibble(
    hydrology,
    "lpjml-soc-hydrology",
    year_col = "year",
    formats = "parquet",
    out_dir = artifact_dir
  )[[1]])

  c(
    "lpjml-grass-natural-net-c" = net_c_path,
    "lpjml-soc-hydrology" = hydrology_path
  )
}

#' Build the crop regime-yield artifact from a finished run.
#'
#' The rainfed and irrigated harvest per unit of each stand's own area, per
#' cell, LPJmL crop and year, with both stand fractions, from WHEP's own
#' reader at the crop grain (`.lrg_crop_yield()`, "others" stand included),
#' so the pinned and run-derived paths cannot drift apart. WHEP expands it to
#' production items on read. A standalone NULL-year call defaults to 1850
#' onward; explicit years are preserved. The shared family entry point passes
#' its resolved common year set explicitly.
prepare_whep_lpjml_regime_artifacts <- function(
  run_dir = default_lpjml_grass_run_dir(),
  whep_path = whep_source_path(),
  years = NULL,
  artifact_dir = fs::path(tempdir(), "whep_lpjml_pins")
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  fs::dir_create(artifact_dir)
  whep <- load_whep_namespace(whep_path)
  cli::cli_alert_info("Preparing LPJmL crop regime yields")
  regime <- build_lpjml_crop_regime_yield(whep, run_dir, years)
  path <- unname(save_processed_tibble(
    regime,
    "lpjml-crop-regime-yield",
    year_col = "year",
    formats = "parquet",
    out_dir = artifact_dir
  )[[1]])
  c("lpjml-crop-regime-yield" = path)
}

# One year at a time, each spilled to disk, for the same reason as
# build_lpjml_soc_hydrology(): a year of cftfrac alone is 6.4 million
# band-cells, and R does not return the transient heap to the OS.
build_lpjml_crop_regime_yield <- function(whep, run_dir, years) {
  if (is.null(years)) {
    years <- whep$.lrg_run_years(run_dir)
    years <- years[years >= 1850L]
  }
  cli::cli_alert_info(
    "Crop regime yields: {length(years)} years, one at a time"
  )
  spill <- fs::path(tempdir(), "crop_regime_yield_years")
  fs::dir_create(spill)
  on.exit(fs::dir_delete(spill), add = TRUE)
  for (i in seq_along(years)) {
    if (i %% 25L == 0L) {
      cli::cli_alert_info("  ...{years[[i]]} ({i}/{length(years)})")
    }
    part <- whep$.lrg_crop_yield(
      years = years[[i]],
      run_dir = run_dir,
      include_others = TRUE
    )
    nanoparquet::write_parquet(
      part,
      fs::path(spill, paste0(years[[i]], ".parquet"))
    )
    rm(part)
    invisible(gc(verbose = FALSE))
  }
  files <- fs::dir_ls(spill, glob = "*.parquet")
  data.table::rbindlist(lapply(files, nanoparquet::read_parquet)) |>
    tibble::as_tibble()
}

#' Build the crop irrigation-depth artifact from a finished run.
#'
#' The annual irrigation LPJmL applied to each crop's irrigated and rainfed
#' stand (mm per square metre of that stand), per cell, LPJmL crop and year,
#' with both stand fractions and the run's vintage (model version, git hash,
#' run name and start time), from WHEP's own reader at the crop grain
#' (`.lid_crop_depth()`, "others" stand included), so the pinned and
#' run-derived paths cannot drift apart. WHEP maps it to items and water
#' regimes on read. It needs a run that writes `cft_airrig_month.nc` (LPJmL
#' runs from 2026-08-31 on); WHEP's reader refuses an older one. A standalone
#' NULL-year call defaults to 1850 onward; explicit years are preserved.
prepare_whep_lpjml_irrig_depth_artifacts <- function(
  run_dir = default_lpjml_grass_run_dir(),
  whep_path = whep_source_path(),
  years = NULL,
  artifact_dir = fs::path(tempdir(), "whep_lpjml_pins")
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  fs::dir_create(artifact_dir)
  whep <- load_whep_namespace(whep_path)
  cli::cli_alert_info("Preparing LPJmL crop irrigation depth")
  irrig <- build_lpjml_crop_irrig_depth(whep, run_dir, years)
  path <- unname(save_processed_tibble(
    irrig,
    "lpjml-crop-irrig-depth",
    year_col = "year",
    formats = "parquet",
    out_dir = artifact_dir
  )[[1]])
  c("lpjml-crop-irrig-depth" = path)
}

# One year at a time, each spilled to disk, for the same reason as
# build_lpjml_soc_hydrology(): a year of cft_airrig_month alone is 7.7e7
# long-form band-cell-months, and R does not return the transient heap to the
# OS.
build_lpjml_crop_irrig_depth <- function(whep, run_dir, years) {
  if (is.null(years)) {
    years <- whep$.lid_run_years(run_dir)
    years <- years[years >= 1850L]
  }
  cli::cli_alert_info(
    "Crop irrigation depth: {length(years)} years, one at a time"
  )
  spill <- fs::path(tempdir(), "crop_irrig_depth_years")
  fs::dir_create(spill)
  on.exit(fs::dir_delete(spill), add = TRUE)
  for (i in seq_along(years)) {
    if (i %% 25L == 0L) {
      cli::cli_alert_info("  ...{years[[i]]} ({i}/{length(years)})")
    }
    part <- whep$.lid_crop_depth(years = years[[i]], run_dir = run_dir)
    nanoparquet::write_parquet(
      part,
      fs::path(spill, paste0(years[[i]], ".parquet"))
    )
    rm(part)
    invisible(gc(verbose = FALSE))
  }
  files <- fs::dir_ls(spill, glob = "*.parquet")
  data.table::rbindlist(lapply(files, nanoparquet::read_parquet)) |>
    tibble::as_tibble()
}


#' Build the annual deep-seepage artifact from a finished run.
#'
#' Annual LPJmL deep seepage (`mseepage.nc`, mm per year, the twelve months
#' summed) per land cell and year, with the run's vintage (model version, git
#' hash, run name and start time), from WHEP's own pin seam
#' (`.hydro_seepage_layer()`), so the pinned and run-derived values cannot
#' drift apart. Seepage is a whole-cell quantity: LPJmL has no per-crop
#' seepage. A standalone NULL-year call defaults to 1850 onward; explicit
#' years are preserved.
prepare_whep_lpjml_seepage_artifacts <- function(
  run_dir = default_lpjml_grass_run_dir(),
  whep_path = whep_source_path(),
  years = NULL,
  artifact_dir = fs::path(tempdir(), "whep_lpjml_pins")
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  fs::dir_create(artifact_dir)
  whep <- load_whep_namespace(whep_path)
  cli::cli_alert_info("Preparing LPJmL deep seepage")
  seepage <- build_lpjml_seepage(whep, run_dir, years)
  path <- unname(save_processed_tibble(
    seepage,
    "lpjml-seepage",
    year_col = "year",
    formats = "parquet",
    out_dir = artifact_dir
  )[[1]])
  c("lpjml-seepage" = path)
}

# One year at a time, each spilled to disk, for the same reason as
# build_lpjml_soc_hydrology(): R does not return the transient heap of the
# monthly cube (2.4e6 cell-months a year) to the OS.
build_lpjml_seepage <- function(whep, run_dir, years) {
  if (is.null(years)) {
    years <- lpjml_seepage_years(run_dir)
    years <- years[years >= 1850L]
  }
  cli::cli_alert_info("Deep seepage: {length(years)} years, one at a time")
  spill <- fs::path(tempdir(), "seepage_years")
  fs::dir_create(spill)
  on.exit(fs::dir_delete(spill), add = TRUE)
  for (i in seq_along(years)) {
    if (i %% 25L == 0L) {
      cli::cli_alert_info("  ...{years[[i]]} ({i}/{length(years)})")
    }
    part <- whep$.hydro_seepage_layer(run_dir, years = years[[i]])
    nanoparquet::write_parquet(
      part,
      fs::path(spill, paste0(years[[i]], ".parquet"))
    )
    rm(part)
    invisible(gc(verbose = FALSE))
  }
  files <- fs::dir_ls(spill, glob = "*.parquet")
  data.table::rbindlist(lapply(files, nanoparquet::read_parquet)) |>
    tibble::as_tibble()
}

# Calendar years in mseepage.nc, from its own dated time axis, never assumed:
# the reference run starts in 1750, not 1901.
lpjml_seepage_years <- function(run_dir) {
  path <- fs::path(run_dir, "mseepage.nc")
  if (!fs::file_exists(path)) {
    cli::cli_abort("No {.file mseepage.nc} in {.path {run_dir}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  first_year <- lpjml_run_first_year(run_dir, "mseepage.nc")
  first_year + seq_len(nc$dim[["time"]]$len %/% 12L) - 1L
}


# .gn_net_c_from_lpjml() IS the documented pin seam: its output schema is the
# artifact's schema, so calling it here keeps one definition of the layer.
#
# BUILT IN YEAR BLOCKS, for the same reason build_lpjml_soc_hydrology() is.
# The seam keeps 16 of pft_npp's 43 bands, but reads them all: over the whole
# 1750-2023 span that is ~8e8 long rows before the PFT filter. Observed 59 GB
# resident and still climbing on a 128 GB machine carrying other jobs, which
# is how this came to be chunked. The seam is per cell-year, so the blocks
# concatenate and the artifact is identical; peak stays near 2 GB and the
# whole span takes about 47 minutes.
# The years pft_npp.nc actually holds. Read from THAT file rather than from
# mswc.nc, which lpjml_hydrology_years() uses: the two agree on this run but a
# run whose carbon and water outputs cover different spans would otherwise be
# blocked over the wrong axis, and silently, since every block would still
# return rows.
lpjml_net_carbon_years <- function(run_dir) {
  path <- fs::path(run_dir, "pft_npp.nc")
  if (!fs::file_exists(path)) {
    cli::cli_abort("No {.file pft_npp.nc} in {.path {run_dir}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  n_years <- nc$dim[["time"]]$len
  lpjml_run_first_year(run_dir, "pft_npp.nc") + seq_len(n_years) - 1L
}

build_lpjml_net_carbon <- function(whep, run_dir, years, block = 5L) {
  years <- years %||% lpjml_net_carbon_years(run_dir)
  blocks <- split(years, ceiling(seq_along(years) / block))
  cli::cli_alert_info(
    "Net carbon: {length(years)} year{?s} in {length(blocks)} block{?s}"
  )
  parts <- purrr::map(blocks, function(yrs) {
    out <- tibble::as_tibble(whep$.gn_net_c_from_lpjml(list(), yrs, run_dir))
    invisible(gc(verbose = FALSE))
    out
  })
  dplyr::bind_rows(parts)
}

# The hydrology artifact is the only member with no single reader
# behind it: it is three monthly variables joined on cell-month. Assembled to
# the schema get_soc_climate_drivers() checks for, and topsoil is the shallowest
# SWC layer, matching WHEP's .wb_swc_topsoil().
#
# BUILT ONE YEAR AT A TIME, and that is not premature optimisation -- reading
# the whole span at once gets the process OOM-killed. SWC is 4-D: the reader
# returns every soil layer and the shallowest is selected afterwards, so a
# full-span read materialises 720 x 277 x 6 layers x 1476 months = 1.77e9 rows
# (~66 GB, observed 113 GB resident before the kernel killed it) to keep the
# 86.8e6 that survive the land mask and the layer filter. Chunking caps the peak
# at one year (~0.7e6 output rows) and costs nothing, because the NetCDF reads
# are already sliced by time.
build_lpjml_soc_hydrology <- function(whep, run_dir, years) {
  years <- years %||% lpjml_hydrology_years(run_dir)
  cli::cli_alert_info("SOC hydrology: {length(years)} years, one at a time")

  # Each year is written to DISK and dropped, rather than kept in a list.
  #
  # Chunking alone was not enough. Per year the live result is only ~40 MB
  # (705,540 rows), but reading it costs ~19e6 transient rows -- 14.4e6 of them
  # just the six SWC layers before the shallowest is selected -- and R does not
  # return that heap to the OS. Accumulating in a list grew resident memory
  # ~1.76 GB per year, reaching 43.9 GB by year 25 and heading for ~216 GB.
  # Spilling to parquet caps the peak at one year plus the final assembly.
  spill <- fs::path(tempdir(), "soc_hydrology_years")
  fs::dir_create(spill)
  on.exit(fs::dir_delete(spill), add = TRUE)

  for (i in seq_along(years)) {
    # A plain message every 25 years: a cli progress bar cannot be updated from
    # inside a loop body in another frame.
    if (i %% 25L == 0L) {
      cli::cli_alert_info("  ...{years[[i]]} ({i}/{length(years)})")
    }
    part <- build_lpjml_soc_hydrology_year(whep, run_dir, years[[i]])
    nanoparquet::write_parquet(
      part,
      fs::path(spill, paste0(years[[i]], ".parquet"))
    )
    rm(part)
    invisible(gc(verbose = FALSE))
  }

  files <- fs::dir_ls(spill, glob = "*.parquet")
  data.table::rbindlist(lapply(files, nanoparquet::read_parquet)) |>
    tibble::as_tibble()
}

build_lpjml_soc_hydrology_year <- function(whep, run_dir, year) {
  monthly <- function(var) {
    whep$read_lpjml_hydrology(
      var,
      run_dir = run_dir,
      years = year,
      monthly = TRUE
    ) |>
      tibble::as_tibble()
  }

  swc <- monthly("swc")
  swc <- if (rlang::has_name(swc, "layer")) {
    dplyr::filter(swc, .data$layer == min(.data$layer))
  } else {
    swc
  }
  swc <- dplyr::transmute(
    swc,
    .data$lon,
    .data$lat,
    .data$year,
    .data$month,
    swc_topsoil = .data$value
  )

  prec <- dplyr::transmute(
    monthly("prec"),
    .data$lon,
    .data$lat,
    .data$year,
    .data$month,
    prec_mm = .data$value
  )
  irrig <- dplyr::transmute(
    monthly("irrig"),
    .data$lon,
    .data$lat,
    .data$year,
    .data$month,
    irrig_mm = .data$value
  )

  key <- c("lon", "lat", "year", "month")
  swc |>
    dplyr::inner_join(prec, by = key) |>
    dplyr::inner_join(irrig, by = key) |>
    # Drop ocean. The readers return the full 720 x 277 grid, so 199,440 cells
    # arrive per month of which only 58,795 are land; the rest are fill. The
    # existing pin holds the land mask alone, and keeping the fill rows would
    # inflate the artifact 3.4x and change how every downstream join behaves.
    # Verified against the pin for 2000: 705,540 finite rows on both sides.
    dplyr::filter(
      is.finite(.data$swc_topsoil),
      is.finite(.data$prec_mm),
      is.finite(.data$irrig_mm)
    )
}

# Calendar years present in the run, read from the monthly time axis rather
# than assumed, so a run with a different span chunks correctly.
lpjml_hydrology_years <- function(run_dir, first_year = NULL) {
  path <- fs::path(run_dir, "mswc.nc")
  if (!fs::file_exists(path)) {
    cli::cli_abort("No {.file mswc.nc} in {.path {run_dir}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  n_months <- nc$dim[["time"]]$len
  # The COUNT was always read from the file; the START was assumed to be
  # 1901, which on the 1750-2023 run yields 1901..2174 -- years the run
  # does not contain.
  first_year <- first_year %||% lpjml_run_first_year(run_dir, "mswc.nc")
  first_year + seq_len(n_months %/% 12L) - 1L
}

# WHEP's readers provide the remaining five artifacts. Loaded rather than
# reimplemented so the pin cannot drift from what the package expects.
#
# Returns the ATTACHED package environment, not asNamespace("whep"): one of the
# two layers is built from a private helper (the documented pin seam), and
# pkgload::load_all(export_all = TRUE) puts private objects in the attached
# environment while the namespace holds only the exports. Looking in the
# namespace finds the exported readers but silently misses the seam.
load_whep_namespace <- function(whep_path) {
  whep_path <- fs::path_expand(whep_path)
  if (!fs::dir_exists(whep_path)) {
    cli::cli_abort(c(
      "WHEP source not found at {.path {whep_path}}.",
      i = "Pass {.arg whep_path} or set {.envvar WHEP_SOURCE_PATH}."
    ))
  }
  rlang::check_installed("pkgload")
  suppressMessages(pkgload::load_all(whep_path, quiet = TRUE))

  attached <- "package:whep"
  env <- if (attached %in% search()) {
    as.environment(attached)
  } else {
    asNamespace("whep")
  }
  if (!is.function(env$.gn_net_c_from_lpjml)) {
    cli::cli_abort(c(
      "Loaded WHEP from {.path {whep_path}} but the net-carbon pin seam
       {.fun .gn_net_c_from_lpjml} is not visible.",
      i = "It is a private helper; loading must keep private objects visible."
    ))
  }
  env
}

whep_source_path <- function() {
  env <- Sys.getenv("WHEP_SOURCE_PATH", unset = "")
  if (nzchar(env)) env else "~/WHEP"
}

# Rows, span and size per artifact. A pin with a truncated year span is the
# most likely silent failure here, so the span is printed for every one.
report_lpjml_pin_manifest <- function(paths) {
  cli::cli_h2("Manifest")
  rows <- purrr::imap(paths, function(path, name) {
    d <- nanoparquet::read_parquet(path)
    tibble::tibble(
      pin = name,
      rows = nrow(d),
      first_year = if ("year" %in% names(d)) min(d$year) else NA_integer_,
      last_year = if ("year" %in% names(d)) max(d$year) else NA_integer_,
      mb = round(as.numeric(fs::file_size(path)) / 1024^2, 1)
    )
  })
  print(as.data.frame(dplyr::bind_rows(rows)), row.names = FALSE)
}

# What each artifact would change if uploaded. Reported as both mean and median
# ratio: a mean that moves while the median does not (or moves the other way)
# is a change in the distribution's shape rather than a rescaling, and anything
# downstream that averages the layer will behave differently in the two cases.
#
# Restricted to COMPARE_YEARS, and the restriction is announced rather than
# silent. Joining the full span of the hydrology layer would be 86.8e6 rows
# against 86.8e6 on four keys; a decade is enough to judge a magnitude, which is
# all this is for.
COMPARE_YEARS <- 2000:2010

report_lpjml_pin_comparison <- function(
  paths,
  whep_path,
  years = COMPARE_YEARS
) {
  cli::cli_h2("Change vs the pins these would replace")
  cli::cli_alert_info(
    "Compared over {min(years)}-{max(years)} only, not the full span."
  )
  whep <- load_whep_namespace(whep_path)
  specs <- lpjml_pin_compare_specs()

  for (name in names(paths)) {
    spec <- specs[[name]]
    if (is.null(spec)) {
      next
    }
    old <- tryCatch(
      tibble::as_tibble(whep$whep_read_file(name, type = "parquet")),
      error = function(e) NULL
    )
    if (is.null(old)) {
      cli::cli_alert_warning("{name}: no current pin to compare against")
      next
    }
    new <- tibble::as_tibble(nanoparquet::read_parquet(paths[[name]]))
    if ("year" %in% names(new) && "year" %in% names(old)) {
      new <- dplyr::filter(new, .data$year %in% years)
      old <- dplyr::filter(old, .data$year %in% years)
    }
    cli::cli_alert_info(compare_one_pin(new, old, spec, name))
    rm(new, old)
    invisible(gc(verbose = FALSE))
  }
}

compare_one_pin <- function(new, old, spec, name) {
  key <- intersect(spec$key, intersect(names(new), names(old)))
  pick <- function(d) {
    dplyr::rename(
      dplyr::select(d, dplyr::all_of(c(key, spec$value))),
      v = !!spec$value
    )
  }
  joined <- dplyr::inner_join(
    dplyr::rename(pick(new), new_v = "v"),
    dplyr::rename(pick(old), old_v = "v"),
    by = key
  )
  joined <- dplyr::filter(
    joined,
    is.finite(.data$new_v),
    is.finite(.data$old_v)
  )
  if (nrow(joined) == 0L) {
    return(paste0(name, ": no overlapping rows to compare"))
  }
  ratios <- joined$new_v[joined$old_v != 0] / joined$old_v[joined$old_v != 0]
  sprintf(
    "%s [%s]: %.4g -> %.4g (mean ratio %.3f, median %.3f, n=%d)",
    name,
    spec$value,
    mean(joined$old_v),
    mean(joined$new_v),
    mean(joined$new_v) / mean(joined$old_v),
    stats::median(ratios, na.rm = TRUE),
    nrow(joined)
  )
}

# One representative value column per pin. Not every column is compared: the
# point is a magnitude check a reviewer can judge, not an exhaustive diff.
lpjml_pin_compare_specs <- function() {
  list(
    "lpjml-grass-availability" = list(
      key = c("lon", "lat", "year"),
      value = "grass_avail_dm_t_ha"
    ),
    "lpjml-grass-productivity" = list(
      key = c("lon", "lat", "year"),
      value = "grass_npp"
    ),
    "lpjml-grass-natural-net-c" = list(
      key = c("lon", "lat", "year", "land_use"),
      value = "npp_c_mgc_ha_yr"
    ),
    "lpjml-soc-hydrology" = list(
      key = c("lon", "lat", "year", "month"),
      value = "swc_topsoil"
    ),
    "lpjml-crop-regime-yield" = list(
      key = c("lon", "lat", "year", "lpjml_crop"),
      value = "yield_rainfed"
    ),
    "lpjml-crop-irrig-depth" = list(
      key = c("lon", "lat", "year", "lpjml_crop"),
      value = "irrig_mm_irrigated"
    ),
    "lpjml-seepage" = list(
      key = c("lon", "lat", "year"),
      value = "seepage_mm"
    )
  )
}

# Preflight every file the existing seven producers consume. Optional natural
# litterfall files are checked when that existing producer will use the pair.
lpjml_pin_input_files <- function(run_dir) {
  files <- c(
    "pft_npp.nc" = 1L,
    "pft_harvestc.nc" = 1L,
    "cftfrac.nc" = 1L,
    "mswc.nc" = 12L,
    "mprec.nc" = 12L,
    "mirrig.nc" = 12L,
    "cft_airrig_month.nc" = 12L,
    "mseepage.nc" = 12L
  )
  optional <- c("litfallc_nv.nc", "fpc.nc")
  if (all(fs::file_exists(fs::path(run_dir, optional)))) {
    files <- c(files, stats::setNames(c(1L, 1L), optional))
  }
  files
}

# Decode the same noleap days-since axis used by lpjml_run_first_year(), but
# inspect every timestamp so a missing year or incomplete month set is visible.
lpjml_pin_file_cohort <- function(path, steps) {
  if (!fs::file_exists(path)) {
    cli::cli_abort(
      "Missing LPJmL family input {.file {path}}.",
      class = "lpjml_cohort_missing"
    )
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  attrs <- ncdf4::ncatt_get(nc, 0)
  vintage <- lapply(c("source", "GIT_hash", "history"), function(key) {
    value <- attrs[[key]]
    if (
      is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)
    ) {
      cli::cli_abort(
        "{.file {path}} lacks {.field {key}} provenance.",
        class = "lpjml_cohort_vintage"
      )
    }
    as.character(value)
  })
  names(vintage) <- c("source", "GIT_hash", "history")
  list(vintage = vintage, years = lpjml_pin_axis_years(nc, path, steps))
}

# Current readers index regular noleap day axes from a January-1 origin.
# CF defines noleap/365_day with the usual month lengths and no February 29:
# https://cfconventions.org/Data/cf-conventions/cf-conventions-1.10/cf-conventions.html
lpjml_pin_axis_years <- function(nc, path, steps) {
  time <- nc$dim[["time"]]
  units <- trimws(time$units %||% "")
  origin <- regmatches(
    units,
    regexec(
      "^days since ([0-9]{3,4})-0?1-0?1([ T]0{1,2}:0{1,2}:0{1,2}([.]0+)?)?$",
      units
    )
  )[[1L]]
  calendar <- tolower(ncdf4::ncatt_get(nc, "time", "calendar")$value)
  if (
    length(origin) == 0L ||
      !calendar %in% c("noleap", "365_day")
  ) {
    cli::cli_abort(
      "{.file {path}} needs a noleap day axis from January 1 at midnight.",
      class = "lpjml_cohort_years"
    )
  }
  values <- time$vals
  if (
    length(values) == 0L || any(!is.finite(values)) || any(diff(values) <= 0)
  ) {
    cli::cli_abort(
      "{.file {path}} needs a strictly increasing dated time axis.",
      class = "lpjml_cohort_years"
    )
  }
  years <- as.integer(origin[[2L]]) + as.integer(floor(values / 365))
  native <- unique(years)
  if (any(diff(native) != 1L) || any(table(years) != steps)) {
    cli::cli_abort(
      "{.file {path}} has discontinuous or incomplete native years.",
      class = "lpjml_cohort_years"
    )
  }
  if (steps == 12L) {
    month_starts <- c(
      0,
      cumsum(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
    )
    month <- findInterval(values %% 365, month_starts)
    if (anyDuplicated(paste(years, month))) {
      cli::cli_abort(
        "{.file {path}} needs one timestamp in each native calendar month.",
        class = "lpjml_cohort_years"
      )
    }
  }
  native
}

lpjml_pin_cohort <- function(
  run_dir,
  years = NULL,
  first_year = NULL,
  whep_path = whep_source_path()
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
  files <- lpjml_pin_input_files(run_dir)
  headers <- purrr::imap(files, function(steps, file) {
    lpjml_pin_file_cohort(fs::path(run_dir, file), steps)
  })
  native_start <- min(headers[["pft_npp.nc"]]$years)
  native_starts <- vapply(headers, function(h) min(h$years), integer(1))
  if (any(native_starts != native_start)) {
    cli::cli_abort(
      "LPJmL family inputs have inconsistent native first years.",
      class = "lpjml_cohort_years"
    )
  }
  if (
    !is.null(first_year) && !identical(as.integer(first_year), native_start)
  ) {
    cli::cli_abort(
      "{.arg first_year} disagrees with the native run time axis.",
      class = "lpjml_cohort_years"
    )
  }
  expected <- headers[[1L]]$vintage
  if (
    !all(vapply(
      headers,
      function(h) identical(h$vintage, expected),
      logical(1)
    ))
  ) {
    cli::cli_abort(
      "LPJmL family inputs come from different simulations.",
      class = "lpjml_cohort_vintage"
    )
  }
  if (is.null(years)) {
    years <- headers[[1L]]$years
    complete <- vapply(
      headers,
      function(h) identical(h$years, years),
      logical(1)
    )
  } else {
    if (
      !is.numeric(years) ||
        length(years) == 0L ||
        anyNA(years) ||
        any(!is.finite(years)) ||
        any(years != as.integer(years))
    ) {
      cli::cli_abort(
        "Supply nonempty integer calendar {.arg years}.",
        class = "lpjml_cohort_years"
      )
    }
    years <- sort(unique(as.integer(years)))
    complete <- vapply(headers, function(h) all(years %in% h$years), logical(1))
  }
  if (!all(complete)) {
    cli::cli_abort(
      "LPJmL input year coverage differs: {.file {names(headers)[!complete]}}.",
      class = "lpjml_cohort_years"
    )
  }
  whep <- load_whep_namespace(whep_path)
  list(
    run_dir = normalizePath(run_dir, winslash = "/", mustWork = TRUE),
    first_year = native_start,
    years = years,
    files = headers,
    vintage = list(
      lpjml_version = expected$source,
      lpjml_git_hash = expected$GIT_hash,
      lpjml_run = whep$.lid_run_label(run_dir, expected$history)
    )
  )
}

# Validate the complete local family before comparison or the first upload.
# A reconciled total cannot establish a supplied input: require nonempty rows,
# every requested year and the input cohort's vintage where rows carry it.
validate_lpjml_pin_family <- function(paths, cohort) {
  specs <- lpjml_pin_compare_specs()
  consumer_columns <- lpjml_pin_consumer_columns()
  if (!setequal(names(specs), names(consumer_columns))) {
    cli::cli_abort(
      "Every LPJmL output needs its complete consumer schema contract.",
      class = "lpjml_family_schema"
    )
  }
  if (
    is.null(names(paths)) ||
      anyDuplicated(names(paths)) ||
      !setequal(names(paths), names(specs))
  ) {
    cli::cli_abort(
      "Expected exactly the complete LPJmL output-pin family.",
      class = "lpjml_family_members"
    )
  }
  rows <- purrr::imap(paths, function(path, pin) {
    if (!fs::file_exists(path)) {
      cli::cli_abort(
        "Missing generated artifact {.val {pin}}.",
        class = "lpjml_family_members"
      )
    }
    d <- nanoparquet::read_parquet(path)
    if (
      !rlang::has_name(d, "year") ||
        nrow(d) == 0L ||
        anyNA(d$year) ||
        !setequal(unique(d$year), cohort$years)
    ) {
      cli::cli_abort(
        "{.val {pin}} lacks the requested year coverage.",
        class = "lpjml_family_coverage"
      )
    }
    required <- c(specs[[pin]]$key, specs[[pin]]$value, consumer_columns[[pin]])
    if (!all(rlang::has_name(d, required))) {
      cli::cli_abort(
        "{.val {pin}} lacks its consumer key/value schema.",
        class = "lpjml_family_schema"
      )
    }
    if (
      pin == "lpjml-grass-natural-net-c" &&
        any(d$land_use == "grassland", na.rm = TRUE) &&
        !rlang::has_name(d, "net_c_mgc_ha_yr")
    ) {
      cli::cli_abort(
        "{.val {pin}} lacks the grassland net-carbon field.",
        class = "lpjml_family_schema"
      )
    }
    if (
      pin %in%
        c("lpjml-crop-irrig-depth", "lpjml-seepage") &&
        !all(rlang::has_name(d, names(cohort$vintage)))
    ) {
      cli::cli_abort(
        "{.val {pin}} lacks its required run vintage.",
        class = "lpjml_family_vintage"
      )
    }
    for (key in intersect(names(cohort$vintage), names(d))) {
      if (anyNA(d[[key]]) || !all(d[[key]] == cohort$vintage[[key]])) {
        cli::cli_abort(
          "{.val {pin}} disagrees with the input {.field {key}}.",
          class = "lpjml_family_vintage"
        )
      }
    }
    tibble::tibble(
      pin = pin,
      path = unname(path),
      rows = nrow(d),
      first_year = min(d$year),
      last_year = max(d$year),
      years = paste(sort(unique(d$year)), collapse = ","),
      lpjml_version = cohort$vintage$lpjml_version,
      lpjml_git_hash = cohort$vintage$lpjml_git_hash,
      lpjml_run = cohort$vintage$lpjml_run,
      run_dir = cohort$run_dir,
      input_files = paste(names(cohort$files), collapse = ","),
      md5 = unname(tools::md5sum(path))
    )
  })
  dplyr::bind_rows(rows)
}

# Full consumer requirements, separate from the one-value comparison summary.
# Contracts: WHEP feed_lpjml.R, grass_natural_carbon_inputs.R, water_balance.R,
# and the crop-grain prototypes in lpjml_regime_yield.R/lpjml_irrig_depth.R.
# Natural litterfall/woody-share fields remain optional as in WHEP's reader;
# net_c_mgc_ha_yr is required specifically when grassland rows are supplied.
lpjml_pin_consumer_columns <- function() {
  list(
    "lpjml-grass-availability" = "grass_avail_dm_t_ha",
    "lpjml-grass-productivity" = "grass_npp",
    "lpjml-grass-natural-net-c" = "npp_c_mgc_ha_yr",
    "lpjml-soc-hydrology" = c("swc_topsoil", "prec_mm", "irrig_mm"),
    "lpjml-crop-regime-yield" = c(
      "yield_rainfed",
      "yield_irrigated",
      "stand_frac_rainfed",
      "stand_frac_irrigated",
      "method_regime_yield"
    ),
    "lpjml-crop-irrig-depth" = c(
      "irrig_mm_irrigated",
      "irrig_mm_rainfed",
      "stand_frac_irrigated",
      "stand_frac_rainfed"
    ),
    "lpjml-seepage" = "seepage_mm"
  )
}
