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
