This repository contains helpers to make whep package inputs available publicly. It's mainly targeted to the WHEP main team, to automate the process of preparing inputs for our whep R package.

For the code to work, you must create a `.Renviron` file in the root of this project with the following content:

```
NEXTCLOUD_URL=https://saco.csic.es/
NEXTCLOUD_USER=your_login_username
NEXTCLOUD_PASSWORD=your_app_password
NEXTCLOUD_USER_ID=your_internal_user_id
NEXTCLOUD_WHEP=https://saco.csic.es/public.php/dav/files/nrJ3JGPZyZeQMW8/
```

Notes on the Nextcloud credentials for CSIC (`saco.csic.es`):

- `NEXTCLOUD_PASSWORD` must be an **app password** (Settings → Security → Devices & sessions → Create new app password). The regular login password will not work because the account uses SSO.
- `NEXTCLOUD_USER` is the name you use to log in (e.g. your DNI).
- `NEXTCLOUD_USER_ID` is the internal Nextcloud user id used in WebDAV paths. It is usually different from the login username. To find it, run in R:
  ```r
  r <- httr::GET(
    paste0(Sys.getenv("NEXTCLOUD_URL"), "ocs/v1.php/cloud/user"),
    httr::authenticate(Sys.getenv("NEXTCLOUD_USER"), Sys.getenv("NEXTCLOUD_PASSWORD")),
    httr::add_headers(`OCS-APIRequest` = "true")
  )
  jsonlite::fromJSON(httr::content(r, as = "text"))$ocs$data$id
  ```
  If `NEXTCLOUD_USER_ID` is left unset, it falls back to `NEXTCLOUD_USER`.

This information won't leave your computer. The file is included in `.gitignore`, which means it's harder to include in the repository by mistake.

Your username and password are needed to be able to upload files to the Nextcloud server programatically.

The codes included here should both upload the raw inputs used (original data), just for transparency, and the cleaner format ones that will be used in the whep R package. You can see an example in `R/process_example.R`. I suggest creating new files for new related inputs, following the style of this one.

## Dependencies

There is no `renv` here: the upload path needs `kwb.nextcloud` while the
artifact generators need `data.table` and `pkgload`, and keeping a project
library meant those two sets never coexisted in one session — so
`regenerate_whep_lpjml_pins(upload = TRUE)` could not run in a single call.
Everything now resolves from the user library instead.

Install once, into the user library:

```r
install.packages(c(
  "arrow", "cli", "data.table", "dplyr", "fs", "httr", "nanoparquet",
  "ncdf4", "pins", "pkgload", "purrr", "readr", "rlang", "stringr",
  "tibble", "yaml"
))
# Not on CRAN -- the Nextcloud client the upload uses:
remotes::install_github("KWB-R/kwb.nextcloud")   # pulls kwb.file, kwb.utils
```

`NEXTCLOUD_WHEP` must be set (see `.Renviron`) for anything that touches the
board. Note that `Rscript --vanilla` skips `.Renviron`, so uploads run with a
plain `Rscript`.

## Upload types

Use `R/upload_utils.R` for the shared upload logic. The helpers support three common cases:

- **Tabular inputs** (`csv`, `tsv`, `txt`, `rds`, `rda`, `RData`): read the source table, write a processed `.csv` and `.parquet`, then upload both files as one pin.
- **Prepared files** (`parquet`, `tif`, `tiff`, `nc`, `gpkg`, `geojson`, archives, shapefile sidecars, etc.): upload the files as-is. Use this for runtime artifacts that the WHEP package expects by filename or file extension.
- **Archives**: bundle a directory or a set of files into a single `.tar.gz` pin before upload.

```r
source("R/upload_csv.R")
upload_csv(
  "~/Downloads/Inputs_LandUse_E_All_Data_(Normalized).csv",
  "faostat-landuse"
)
```

```r
source("R/upload_files.R")

# Prepared file upload: good for rasters, NetCDFs, GeoPackages, existing
# archives, shapefile sidecars, and already-prepared Parquet artifacts.
upload_files("~/data/elevation.tif", "elevation-raster")

# Archive upload: good for directories that belong together.
upload_archive("~/data/HWSD", "hwsd")
```

`upload_input()` can also infer a reasonable default:

```r
source("R/upload_utils.R")
upload_input("~/Downloads/table.csv", "some-tabular-pin")
upload_input("~/data/layer.tif", "some-raster-pin")
upload_input("~/data/raw_directory", "some-archived-directory")
```

Parquet files are treated as prepared files by default, not reprocessed tabular inputs. This matters for generated WHEP runtime artifacts such as `country_grid.parquet`: the package should download the exact file it expects. If you deliberately want to normalize a Parquet table into the tabular CSV plus Parquet pin layout, call `upload_input(..., type = "tabular")`.

## WHEP LPJmL-optional inputs

The WHEP branch that makes LPJmL optional expects generated spatialization and LPJmL grass artifacts to exist as pins. The helper in `R/upload_whep_optional_inputs.R` uploads the known spatialization Parquet files from the default local preparation directory:

```r
source("R/upload_whep_optional_inputs.R")

check_whep_spatialize_inputs("~/WHEP/LPJmL_inputs/whep/inputs")
upload_whep_spatialize_inputs("~/WHEP/LPJmL_inputs/whep/inputs")
```

The spatialization aliases are:

```r
whep_spatialize_pin_map()
```

For LPJmL grass availability/productivity artifacts, either build them from the finished LPJmL run or pass already-generated outputs explicitly. The helper uses `WHEP_LPJML_RUN_DIR` when set; on this machine it otherwise falls back to:

```r
"/home/usuario/Nextcloud/WHEP_ERC 2025/Sources/datasets/unclassified_datasets/LPJmL/LPJmL_runs/global_1901-2009_spinup_200_our_inputs_grassland_livestock_npp_vegc_fix"
```

Build Parquet artifacts from `pft_npp.nc` and `cftfrac.nc`, then upload them:

```r
upload_whep_lpjml_grass_from_run()
```

Or prepare the Parquet artifacts without uploading:

```r
prepare_whep_lpjml_grass_artifacts(
  artifact_dir = "~/WHEP/LPJmL_inputs/whep/grass_artifacts"
)
```

If the Parquet artifacts already exist, upload them directly:

```r
upload_whep_lpjml_grass_artifacts(
  availability_path = "~/path/to/lpjml_grass_availability.parquet",
  productivity_path = "~/path/to/lpjml_grass_productivity.parquet"
)
```

This uploads:

- `lpjml-grass-availability`
- `lpjml-grass-productivity`
