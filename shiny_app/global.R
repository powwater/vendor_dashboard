
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
suppressWarnings({
  library(DBI)
  library(RPostgres)
  library(dbx)
  library(urltools)
  library(dbplyr)
  library(dplyr)
  library(DT)
  # library(highcharter)
  library(formattable)
  library(powpolished)
  library(snakecase)
  library(tychobratools)
  library(htmltools)
  library(shiny)
  library(shinycustomloader)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyFeedback)
  library(shinyjs)
  library(shinyWidgets)
  # library(markdown)
  library(yaml)
  library(lubridate)
  library(purrr)
  library(rlang)
  library(tidyselect)
  library(attempt)
  library(googleway)
  library(shinyFiles)
  library(waiter)
  library(shinyscroll)
  library(dialr)
})

# set default options -----------------------------------------------------
options(shiny.trace = FALSE) # set to T to print full shiny operations.
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
options(lubridate.week.start = 1)

# load configuration ------------------------------------------------------

# tell app if in development or not
is_dev <- Sys.getenv("R_CONFIG_ACTIVE", "default") == "default"
is_local <- Sys.getenv('SHINY_PORT') == ""

# enable shiny devmode
if (is_dev && is_local && packageVersion("shiny") >= "1.6.0") shiny::devmode()

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
  # firebase_config = list(
  #   apiKey = app_config$firebase$api_key,
  #   authDomain = app_config$firebase$auth_domain,
  #   projectId = app_config$firebase$project_id
  # ),
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
