This repository contains helpers to make whep package inputs available publicly. It's mainly targeted to the WHEP main team, to automate the process of preparing inputs for our whep R package.

For the code to work, you must create a `.Renviron` file in the root of this project with the following content:

```
NEXTCLOUD_URL=https://saco.csic.es/
NEXTCLOUD_USER=your_username
NEXTCLOUD_PASSWORD=your_password
NEXTCLOUD_WHEP=https://saco.csic.es/public.php/dav/files/nrJ3JGPZyZeQMW8/
```

This information won't leave your computer. The file is included in `.gitignore`, which means it's harder to include in the repository by mistake.

Your username and password are needed to be able to upload files to the Nextcloud server programatically.

The codes included here should both upload the raw inputs used (original data), just for transparency, and the cleaner format ones that will be used in the whep R package. You can see an example in `R/process_example.R`. I suggest creating new files for new related inputs, following the style of this one.
