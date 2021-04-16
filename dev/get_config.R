# usethis::use_git_hook("post-checkout", script = "dev/hooks.R")

get_config <- function() {

  root <- rprojroot::find_rstudio_root_file()
  root_path <- fs::path(root, "config.yml")
  shiny_app_path <- fs::path(root, "shiny_app")
  meta_cache_path <- fs::path(root, "config/config_last_modified")

  if (!fs::dir_exists(dirname(meta_cache_path))) fs::dir_create(dirname(meta_cache_path))

  if (file.exists(root_path)) file.copy(root_path, paste0(fs::path_ext_remove(root_path), "_old.yml"))

  meta <- googledrive::drive_get(
    googledrive::as_id("https://drive.google.com/file/d/115lcGZ-OCtHgaUM_XQYl6IJBBestwV8_/view?usp=sharing")
    )

  last_modified <- purrr::pluck(meta, "drive_resource", 1, "modifiedTime")

  if (!file.exists(root_path) || !file.exists(shiny_app_path)) {
    download <- usethis::ui_yeah("No configuration file detected. Download from GDrive?")
    if (download) {
      requireNamespace("googledrive")
      googledrive::drive_download(
        file = googledrive::as_id("https://drive.google.com/file/d/115lcGZ-OCtHgaUM_XQYl6IJBBestwV8_/view?usp=sharing"),
        path = root_path,
        overwrite = TRUE
      )
      file.copy(root_path, shiny_app_path, overwrite = TRUE)
    }
    usethis::ui_done("config.yml successfully downloaded to {usethis::ui_path(dirname(root_path))}.")
    usethis::ui_done("config.yml successfully copied into {usethis::ui_path('shiny_app')} directory.")
  } else {
    usethis::ui_info("config.yml already in working directory; checking last modified time on googledrive...")
    if (file.exists(meta_cache_path)) {
      hold <- qs::qread(meta_cache_path)
      if (identical(hold, last_modified)) {
        usethis::ui_done("No changes since prior loading, skipping download.")
      } else {
        usethis::ui_warn("config.yml has changed since last loading. Re-downloading from Google Drive...")
        requireNamespace("googledrive")
        googledrive::drive_download(
          file = googledrive::as_id("https://drive.google.com/file/d/115lcGZ-OCtHgaUM_XQYl6IJBBestwV8_/view?usp=sharing"),
          path = root_path,
          overwrite = TRUE
        )
        file.copy(root_path, shiny_app_path, overwrite = TRUE)
        qs::qsave(last_modified, meta_cache_path)
        usethis::ui_done("config.yml successfully downloaded to {usethis::ui_path(dirname(root_path))}.")
        usethis::ui_done("config.yml successfully copied into {usethis::ui_path('shiny_app')} directory.")
      }
    } else {
      googledrive::drive_download(
        file = googledrive::as_id("https://drive.google.com/file/d/115lcGZ-OCtHgaUM_XQYl6IJBBestwV8_/view?usp=sharing"),
        path = root_path,
        overwrite = TRUE
      )
      file.copy(root_path, shiny_app_path, overwrite = TRUE)
      qs::qsave(last_modified, meta_cache_path)
    }
  }
  usethis::ui_todo("Be sure that config.yml is included in your {usethis::ui_path('.gitignore')}!")
}
