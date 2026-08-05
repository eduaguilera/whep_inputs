# One entry point for every WHEP input pin that is derived from LPJmL model
# OUTPUT. Call regenerate_whep_lpjml_pins() after a new LPJmL run; nothing else
# needs calling, and no other pin needs touching.
#
# WHY ONE ENTRY POINT
#
# There are exactly four such pins, produced by two different generators that
# used to be invoked separately:
#
#   lpjml-grass-availability     read by R/feed_lpjml.R
#   lpjml-grass-productivity     read by R/feed_lpjml.R
#   lpjml-grass-natural-net-c    read by R/grass_natural_carbon_inputs.R
#   lpjml-soc-hydrology          read by R/water_balance.R
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
# Default is generate-and-report only. These four pins are the default source
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
#' @param artifact_dir Where to write the four parquet files.
#' @param years Optional year subset. `NULL` (default) does the whole run, which
#'   is what a pin needs -- pass a subset only for a smoke test.
#' @param compare If `TRUE`, report how each artifact differs from the pin it
#'   would replace. This is the point of the dry run, so it defaults on.
#' @param upload If `TRUE`, upload all four. Defaults to `FALSE`.
#' @return A named character vector of the four local paths, invisibly.
regenerate_whep_lpjml_pins <- function(
  run_dir = default_lpjml_grass_run_dir(),
  whep_path = whep_source_path(),
  artifact_dir = fs::path(tempdir(), "whep_lpjml_pins"),
  years = NULL,
  first_year = 1901L,
  shares = lpjml_grass_access_shares(),
  compare = TRUE,
  upload = FALSE,
  remote_path = default_whep_inputs_path()
) {
  run_dir <- resolve_lpjml_output_dir(run_dir)
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
  paths <- c(grass, soc)

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
    upload_input(local_path, pin_name, remote_path = remote_path, type = "files")
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
  # .gn_net_c_from_lpjml() IS the documented pin seam: its output schema is the
  # artifact's schema, so calling it here keeps one definition of the layer.
  net_c <- whep$.gn_net_c_from_lpjml(list(), years, run_dir) |>
    tibble::as_tibble()
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

# The hydrology artifact is the only one of the four with no single reader
# behind it: it is three monthly variables joined on cell-month. Assembled to
# the schema get_soc_climate_drivers() checks for, and topsoil is the shallowest
# SWC layer, matching WHEP's .wb_swc_topsoil().
build_lpjml_soc_hydrology <- function(whep, run_dir, years) {
  monthly <- function(var) {
    whep$read_lpjml_hydrology(
      var,
      run_dir = run_dir,
      years = years,
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
    dplyr::inner_join(irrig, by = key)
}

# WHEP's readers are needed for two of the four artifacts. Loaded rather than
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
report_lpjml_pin_comparison <- function(paths, whep_path) {
  cli::cli_h2("Change vs the pins these would replace")
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
    cli::cli_alert_info(compare_one_pin(new, old, spec, name))
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
  joined <- dplyr::filter(joined, is.finite(.data$new_v), is.finite(.data$old_v))
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
    )
  )
}
