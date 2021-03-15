# usethis::use_git_hook("post-checkout", script = "dev/hooks.R")

get_config <- function() {

  if (!file.exists("config.yml") && !file.exists("shiny_app/config.yml")) {

    download <- usethis::ui_yeah("No configuration file detected. Download from GDrive?")

    if (download) {
      requireNamespace("googledrive")
      # auth <- googledrive::drive_has_token()
      googledrive::drive_auth()
      googledrive::drive_download(
        file = googledrive::as_id("https://drive.google.com/file/d/115lcGZ-OCtHgaUM_XQYl6IJBBestwV8_/view?usp=sharing"),
        path = "config.yml"
      )
      file.copy("config.yml", "shiny_app/config.yml", overwrite = TRUE)
    }
  } else {
    usethis::ui_done("config.yml already in working directory.")
  }

}


