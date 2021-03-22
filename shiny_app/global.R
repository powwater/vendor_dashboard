
#  ------------------------------------------------------------------------
#
# Title : Global
#    By : Jimmy Briggs
#  Date : 2021-03-09
#
#  ------------------------------------------------------------------------

# NOTE
# For config.yml source R/get_config.R and run get_config() OR pull directly from
# GCP secret manager via dev/gcloud_secret.(ps1|.sh)

# clear global environment ------------------------------------------------
rm(list = ls())

# library packages --------------------------------------------------------
require(pacman)

pacman::p_load(
  DBI,
  dbplyr,
  dplyr,
  DT,
  formattable,
  powpolished,
  snakecase,
  tychobratools,
  RPostgres,
  htmltools,
  shiny,
  shinycustomloader,
  shinydashboard,
  shinydashboardPlus,
  shinyFeedback,
  shinyjs,
  shinyWidgets,
  markdown,
  yaml,
  lubridate,
  purrr,
  rlang,
  tidyselect,
  attempt,
  googleway,
  shinyFiles,
  waiter,
  shinyscroll,
  dialr
)

# set default options -----------------------------------------------------
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# load configuration ------------------------------------------------------

# tell app if in development or not
is_dev <- Sys.getenv("R_CONFIG_ACTIVE", "default") == "default"

# enable shiny devmode
if (is_dev && packageVersion("shiny") >= "1.6.0") shiny::devmode()

# load config yaml file
attempt::attempt({
  app_config <- config::get()
}, msg = "Error loading configuration. Run `get_config()` to
          retrieve config.yml from Google Drive and re-try.")

# setup google maps API key for googleway
key <- app_config$gcp$gmaps_api_key
set_key(key = key)

# setup database connection -----------------------------------------------

conn <- dbx::dbxConnect(app_config$db$url)

# disconnect --------------------------------------------------------------
shiny::onStop({
  function() dbx::dbxDisconnect(conn)
})

# setup powpolished -------------------------------------------------------

powpolished::global_sessions_config(
  api_url = app_config$powpolished$api_url,
  app_name = app_config$powpolished$app_name,
  api_key = app_config$powpolished$api_key,
  firebase_config = list(
    apiKey = app_config$firebase$api_key,
    authDomain = app_config$firebase$auth_domain,
    projectId = app_config$firebase$project_id
  ),
  sign_in_providers = c("google",
                        "microsoft",
                        "facebook",
                        "email")
)

# assets ---------------------------------------

assets <- yaml::read_yaml("config/assets.yml")
pow_colors <- assets$colors

# choices -----------------------------------------------------------------
choices <- yaml::read_yaml("config/choices.yml") %>%
  purrr::map(unlist)
