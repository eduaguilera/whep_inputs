# Run offline from the repository root:
# Rscript -e 'testthat::test_file("tests/test_lpjml_regeneration.R")'
# Source libraries explicitly: process_example.R publishes on source.
test_root <- normalizePath(if (file.exists("R/upload_utils.R")) "." else "..")
withr::local_dir(test_root)
regeneration_env <- new.env(parent = globalenv())
sys.source("R/upload_utils.R", envir = regeneration_env)
sys.source("R/upload_whep_optional_inputs.R", envir = regeneration_env)
sys.source("R/regenerate_whep_lpjml_pins.R", envir = regeneration_env)

# Header validation is real; only the WHEP run-label formatter is stubbed.
regeneration_env$load_whep_namespace <- function(...) {
  list(.lid_run_label = function(run_dir, history) {
    paste(basename(run_dir), history)
  })
}

# WHEP output-pin catalog at 3201ad1e; climate forcing is a separate family.
output_pins <- c(
  "lpjml-grass-availability",
  "lpjml-grass-productivity",
  "lpjml-grass-natural-net-c",
  "lpjml-soc-hydrology",
  "lpjml-crop-regime-yield",
  "lpjml-crop-irrig-depth",
  "lpjml-seepage"
)
forcing_pins <- c(
  "lpjml-wind-isimip-1901-2019",
  "lpjml-wind-era5-2017-2023",
  "lpjml-rsds-isimip-1901-2019",
  "lpjml-rsds-era5-2017-2023",
  "lpjml-rlds-isimip-1901-2019",
  "lpjml-rlds-era5-2017-2023"
)

make_run_fixture <- function(years = 2010:2011, changed = NULL) {
  run <- tempfile("lpjml-run-")
  dir.create(run)
  steps <- c(
    "pft_npp.nc" = 1L,
    "pft_harvestc.nc" = 1L,
    "cftfrac.nc" = 1L,
    "mswc.nc" = 12L,
    "mprec.nc" = 12L,
    "mirrig.nc" = 12L,
    "cft_airrig_month.nc" = 12L,
    "mseepage.nc" = 12L
  )
  for (file in names(steps)) {
    yy <- if (identical(file, changed)) tail(years, 1L) else years
    time <- unlist(lapply(yy, function(y) {
      offset <- if (steps[[file]] == 12L) {
        c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
      } else {
        182
      }
      (y - min(years)) * 365 + offset
    }))
    dim <- ncdf4::ncdim_def(
      "time",
      paste0("days since ", min(years), "-01-01"),
      time
    )
    var <- ncdf4::ncvar_def("value", "1", list(dim), -9999)
    nc <- ncdf4::nc_create(file.path(run, file), var)
    ncdf4::ncatt_put(nc, "time", "calendar", "noleap")
    ncdf4::ncatt_put(nc, 0, "source", "LPJmL test version")
    ncdf4::ncatt_put(nc, 0, "GIT_hash", "test-revision")
    ncdf4::ncatt_put(nc, 0, "history", "Mon Aug 31 14:13:10 2026: lpjml")
    ncdf4::nc_close(nc)
  }
  run
}

# Independent fixture schemas from WHEP3201ad1e: feed_lpjml.R normalisers,
# grass_natural_carbon_inputs.R grassland guard, water_balance.R hydrology,
# and the crop-grain prototypes in lpjml_regime_yield.R/lpjml_irrig_depth.R.
consumer_fixture_columns <- list(
  "lpjml-grass-availability" = c("grass_avail_dm_t_ha"),
  "lpjml-grass-productivity" = c("grass_npp"),
  "lpjml-grass-natural-net-c" = c("npp_c_mgc_ha_yr", "net_c_mgc_ha_yr"),
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

make_family_fixture <- function(cohort) {
  out <- tempfile("lpjml-family-")
  dir.create(out)
  specs <- regeneration_env$lpjml_pin_compare_specs()
  paths <- vapply(
    names(specs),
    function(pin) {
      spec <- specs[[pin]]
      d <- tibble::tibble(year = cohort$years)
      for (key in setdiff(spec$key, "year")) {
        d[[key]] <- 1L
      }
      for (column in consumer_fixture_columns[[pin]]) {
        d[[column]] <- if (column == "method_regime_yield") {
          "fixture_method"
        } else {
          seq_along(cohort$years)
        }
      }
      if ("land_use" %in% names(d)) {
        d$land_use <- "grassland"
      }
      if ("lpjml_crop" %in% names(d)) {
        d$lpjml_crop <- "maize"
      }
      if (pin %in% c("lpjml-crop-irrig-depth", "lpjml-seepage")) {
        for (key in names(cohort$vintage)) {
          d[[key]] <- cohort$vintage[[key]]
        }
      }
      path <- file.path(out, paste0(pin, ".parquet"))
      nanoparquet::write_parquet(d, path)
      path
    },
    character(1)
  )
  paths
}

testthat::test_that("all consumed outputs and no forcing have comparison specs", {
  pins <- names(regeneration_env$lpjml_pin_compare_specs())
  testthat::expect_setequal(pins, output_pins)
  testthat::expect_false(any(forcing_pins %in% pins))
})

testthat::test_that("one cohort preserves sparse requested years and native start", {
  run <- make_run_fixture(1849:1851)
  cohort <- regeneration_env$lpjml_pin_cohort(run, c(1849L, 1851L))
  testthat::expect_identical(cohort$years, c(1849L, 1851L))
  testthat::expect_identical(cohort$first_year, 1849L)
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(run, 1901L),
    class = "lpjml_cohort_years"
  )
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(run, 1850L, first_year = 1901L),
    class = "lpjml_cohort_years"
  )
})

testthat::test_that("different vintage and incomplete source years fail before build", {
  run <- make_run_fixture()
  nc <- ncdf4::nc_open(file.path(run, "mseepage.nc"), write = TRUE)
  ncdf4::ncatt_put(nc, 0, "GIT_hash", "other-revision")
  ncdf4::nc_close(nc)
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(run, 2010L),
    class = "lpjml_cohort_vintage"
  )
  truncated <- make_run_fixture(changed = "cft_airrig_month.nc")
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(truncated, 2010:2011),
    class = "lpjml_cohort_years"
  )
})

testthat::test_that("manifest refuses missing, extra, empty and wrong-year members", {
  cohort <- regeneration_env$lpjml_pin_cohort(make_run_fixture(), 2010:2011)
  paths <- make_family_fixture(cohort)
  manifest <- regeneration_env$validate_lpjml_pin_family(paths, cohort)
  testthat::expect_equal(nrow(manifest), 7L)
  testthat::expect_setequal(manifest$pin, output_pins)
  testthat::expect_error(
    regeneration_env$validate_lpjml_pin_family(paths[-1L], cohort),
    class = "lpjml_family_members"
  )
  testthat::expect_error(
    regeneration_env$validate_lpjml_pin_family(
      c(paths, stats::setNames(paths[[1L]], forcing_pins[[1L]])),
      cohort
    ),
    class = "lpjml_family_members"
  )
  empty <- nanoparquet::read_parquet(paths[[1L]])[0, ]
  nanoparquet::write_parquet(empty, paths[[1L]])
  testthat::expect_error(
    regeneration_env$validate_lpjml_pin_family(paths, cohort),
    class = "lpjml_family_coverage"
  )
  paths <- make_family_fixture(cohort)
  short <- nanoparquet::read_parquet(paths[[1L]])[1L, ]
  nanoparquet::write_parquet(short, paths[[1L]])
  testthat::expect_error(
    regeneration_env$validate_lpjml_pin_family(paths, cohort),
    class = "lpjml_family_coverage"
  )
})

testthat::test_that("artifact vintage cannot disagree with its input cohort", {
  cohort <- regeneration_env$lpjml_pin_cohort(make_run_fixture(), 2010L)
  paths <- make_family_fixture(cohort)
  path <- paths[["lpjml-seepage"]]
  d <- nanoparquet::read_parquet(path)
  d$lpjml_git_hash <- "other-revision"
  nanoparquet::write_parquet(d, path)
  testthat::expect_error(
    regeneration_env$validate_lpjml_pin_family(paths, cohort),
    class = "lpjml_family_vintage"
  )
})

testthat::test_that("artifact schema and required vintage are supplied", {
  cohort <- regeneration_env$lpjml_pin_cohort(make_run_fixture(), 2010L)
  paths <- make_family_fixture(cohort)
  path <- paths[["lpjml-seepage"]]
  d <- nanoparquet::read_parquet(path)
  d$seepage_mm <- NULL
  nanoparquet::write_parquet(d, path)
  testthat::expect_error(
    regeneration_env$validate_lpjml_pin_family(paths, cohort),
    class = "lpjml_family_schema"
  )
  paths <- make_family_fixture(cohort)
  path <- paths[["lpjml-seepage"]]
  d <- nanoparquet::read_parquet(path)
  d$lpjml_run <- NULL
  nanoparquet::write_parquet(d, path)
  testthat::expect_error(
    regeneration_env$validate_lpjml_pin_family(paths, cohort),
    class = "lpjml_family_vintage"
  )
})

testthat::test_that("existing producer seams preserve explicit pre-1850 years", {
  whep <- list(
    .lrg_run_years = function(run_dir) 1849:1851,
    .lid_run_years = function(run_dir) 1849:1851,
    .lrg_crop_yield = function(years, run_dir, include_others) {
      testthat::expect_true(include_others)
      tibble::tibble(year = years, value = years * 2)
    },
    .lid_crop_depth = function(years, run_dir) {
      tibble::tibble(year = years, value = years * 3)
    },
    .hydro_seepage_layer = function(run_dir, years) {
      tibble::tibble(year = years, value = years * 4)
    }
  )
  run <- make_run_fixture(1849:1851)
  builders <- list(
    regeneration_env$build_lpjml_crop_regime_yield,
    regeneration_env$build_lpjml_crop_irrig_depth,
    regeneration_env$build_lpjml_seepage
  )
  for (builder in builders) {
    explicit <- builder(whep, run, c(1849L, 1851L))
    testthat::expect_setequal(explicit$year, c(1849L, 1851L))
    default <- builder(whep, run, NULL)
    testthat::expect_setequal(default$year, 1850:1851)
  }
})

testthat::test_that("orchestration validates every member before any upload", {
  run <- make_run_fixture()
  cohort <- regeneration_env$lpjml_pin_cohort(run, 2010:2011)
  paths <- make_family_fixture(cohort)
  e <- new.env(parent = regeneration_env)
  e$regenerate_whep_lpjml_pins <- regeneration_env$regenerate_whep_lpjml_pins
  environment(e$regenerate_whep_lpjml_pins) <- e
  calls <- new.env(parent = emptyenv())
  calls$args <- list()
  calls$uploads <- character()
  e$prepare_whep_lpjml_grass_artifacts <- function(...) {
    calls$args$grass <- list(...)
    paths[output_pins[1:2]]
  }
  e$prepare_whep_lpjml_soc_artifacts <- function(...) {
    calls$args$soc <- list(...)
    paths[output_pins[3:4]]
  }
  e$prepare_whep_lpjml_regime_artifacts <- function(...) {
    calls$args$regime <- list(...)
    paths[output_pins[5L]]
  }
  e$prepare_whep_lpjml_irrig_depth_artifacts <- function(...) {
    calls$args$irrig <- list(...)
    paths[output_pins[6L]]
  }
  e$prepare_whep_lpjml_seepage_artifacts <- function(...) {
    calls$args$seepage <- list(...)
    paths[output_pins[7L]]
  }
  e$report_lpjml_pin_manifest <- function(paths) invisible(NULL)
  e$report_lpjml_pin_comparison <- function(paths, whep_path) {
    calls$compared <- names(paths)
  }
  e$upload_input <- function(local_path, pin_name, ...) {
    calls$uploads <- c(calls$uploads, pin_name)
  }
  dest <- tempfile("lpjml-dry-run-")
  out <- suppressMessages(e$regenerate_whep_lpjml_pins(
    run_dir = run,
    whep_path = "fixture",
    years = 2010:2011,
    shares = list(),
    artifact_dir = dest
  ))
  testthat::expect_setequal(names(out), output_pins)
  testthat::expect_setequal(calls$compared, output_pins)
  testthat::expect_length(calls$uploads, 0L)
  testthat::expect_true(file.exists(file.path(
    dest,
    "lpjml-family-manifest.csv"
  )))
  for (args in calls$args) {
    testthat::expect_identical(args$run_dir, run)
    testthat::expect_identical(args$years, 2010:2011)
  }
  suppressMessages(e$regenerate_whep_lpjml_pins(
    run_dir = run,
    whep_path = "fixture",
    years = 2010:2011,
    shares = list(),
    artifact_dir = dest,
    upload = TRUE
  ))
  testthat::expect_setequal(calls$uploads, output_pins)
  calls$uploads <- character()
  e$prepare_whep_lpjml_seepage_artifacts <- function(...) character()
  testthat::expect_error(
    suppressMessages(e$regenerate_whep_lpjml_pins(
      run_dir = run,
      whep_path = "fixture",
      years = 2010:2011,
      shares = list(),
      artifact_dir = dest,
      upload = TRUE
    )),
    class = "lpjml_family_members"
  )
  testthat::expect_length(calls$uploads, 0L)
})

testthat::test_that("missing provenance and inputs cannot become a partial cohort", {
  run <- make_run_fixture()
  unlink(file.path(run, "mseepage.nc"))
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(run, 2010L),
    class = "lpjml_cohort_missing"
  )
  run <- make_run_fixture()
  nc <- ncdf4::nc_open(file.path(run, "mseepage.nc"), write = TRUE)
  ncdf4::ncatt_put(nc, 0, "source", "")
  ncdf4::nc_close(nc)
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(run, 2010L),
    class = "lpjml_cohort_vintage"
  )
  truncated <- make_run_fixture(changed = "cft_airrig_month.nc")
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(truncated),
    class = "lpjml_cohort_years"
  )
  run <- make_run_fixture()
  for (years in list(NA_integer_, Inf, "2010", integer())) {
    testthat::expect_error(
      regeneration_env$lpjml_pin_cohort(run, years),
      class = "lpjml_cohort_years"
    )
  }
})

testthat::test_that("axis units, calendar, order and native continuity are checked", {
  for (change in list(
    c("units", "hours since 2010-01-01"),
    c("units", "days since 2010-02-01"),
    c("calendar", "gregorian"),
    c("calendar", "360_day"),
    c("calendar", "")
  )) {
    run <- make_run_fixture()
    nc <- ncdf4::nc_open(file.path(run, "mseepage.nc"), write = TRUE)
    ncdf4::ncatt_put(nc, "time", change[[1L]], change[[2L]])
    ncdf4::nc_close(nc)
    testthat::expect_error(
      regeneration_env$lpjml_pin_cohort(run, 2010L),
      class = "lpjml_cohort_years"
    )
  }
  run <- make_run_fixture()
  nc <- ncdf4::nc_open(file.path(run, "pft_npp.nc"), write = TRUE)
  ncdf4::ncvar_put(nc, "time", rev(nc$dim$time$vals))
  ncdf4::nc_close(nc)
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(run, 2010:2011),
    class = "lpjml_cohort_years"
  )
  shared_gap <- make_run_fixture(c(2010L, 2012L))
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(shared_gap, c(2010L, 2012L)),
    class = "lpjml_cohort_years"
  )
  run <- make_run_fixture()
  nc <- ncdf4::nc_open(file.path(run, "mseepage.nc"), write = TRUE)
  vals <- nc$dim$time$vals
  vals[[2L]] <- 20 # Twelve unique samples, but January twice and no February.
  ncdf4::ncvar_put(nc, "time", vals)
  ncdf4::nc_close(nc)
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(run, 2010:2011),
    class = "lpjml_cohort_years"
  )
  later_start <- make_run_fixture(changed = "cft_airrig_month.nc")
  testthat::expect_error(
    regeneration_env$lpjml_pin_cohort(later_start, 2011L),
    class = "lpjml_cohort_years"
  )
})

testthat::test_that("every non-key consumer field is required before upload", {
  cohort <- regeneration_env$lpjml_pin_cohort(make_run_fixture(), 2010L)
  paths <- make_family_fixture(cohort)
  for (pin in names(consumer_fixture_columns)) {
    path <- paths[[pin]]
    original <- nanoparquet::read_parquet(path)
    for (column in consumer_fixture_columns[[pin]]) {
      missing <- original
      missing[[column]] <- NULL
      nanoparquet::write_parquet(missing, path)
      testthat::expect_error(
        regeneration_env$validate_lpjml_pin_family(paths, cohort),
        class = "lpjml_family_schema",
        info = paste(pin, column)
      )
    }
    nanoparquet::write_parquet(original, path)
  }
})

testthat::test_that("every registered output has a complete consumer contract", {
  cohort <- regeneration_env$lpjml_pin_cohort(make_run_fixture(), 2010L)
  paths <- make_family_fixture(cohort)
  e <- new.env(parent = regeneration_env)
  e$validate_lpjml_pin_family <- regeneration_env$validate_lpjml_pin_family
  environment(e$validate_lpjml_pin_family) <- e
  e$lpjml_pin_consumer_columns <- function() list()
  testthat::expect_error(
    e$validate_lpjml_pin_family(paths, cohort),
    class = "lpjml_family_schema"
  )
})
