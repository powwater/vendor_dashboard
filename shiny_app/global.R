#  ------------------------------------------------------------------------
#
# Title : Global
#    By : Jimmy Briggs
#  Date : 2021-03-09
#
#  ------------------------------------------------------------------------

# clear global environment ------------------------------------------------
rm(list = ls())

# library packages --------------------------------------------------------
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dbplyr)
  library(dplyr)
  library(DT)
  library(formattable)
  library(powpolished)
  library(snakecase)
  library(tychobratools)
  library(htmltools)
  library(shiny)
  library(shinycustomloader)
  library(shinydashboard)
  library(shinydashboardPlus) # v0.7.5!
  library(shinyFeedback)
  library(shinyjs)
  library(shinyWidgets)
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
  # library(logger)
})

if (packageVersion("shinydashboardPlus") > "0.7.5") {
  usethis::ui_stop(
    paste0(
      "package `shinydashboardPlus` must use version 0.7.5; run ",
      usethis::ui_code("remotes::install_version('shinydashboardPlus', '0.7.5')"),
      " to install."
    )
  )
}

# set default options -----------------------------------------------------
options(shiny.trace = FALSE) # set to T to print full shiny operations.
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
options(lubridate.week.start = 1)

# load configuration ------------------------------------------------------

# tell app if in development or not
is_dev <- Sys.getenv("R_CONFIG_ACTIVE", "default") %in% c("default", "local")
is_local <- Sys.getenv('SHINY_PORT') == ""

# # run local docker database for development
# docker_db <- TRUE
# if (docker_db) Sys.setenv("R_CONFIG_ACTIVE" = "local")
#
# # download latest config.yml file:
# if (is_local) source("R/get_config.R"); get_config()

# enable shiny devmode
if (is_dev && is_local && packageVersion("shiny") >= "1.6.0") shiny::devmode()

# load config yaml file
app_config <- config::get(file = "config.yml")

# setup google maps API key for googleway
key <- app_config$gcp$gmaps_api_key
set_key(key = key)

# setup database connection -----------------------------------------------

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = app_config$db$host,
  dbname = app_config$db$dbname,
  user = app_config$db$user,
  password = app_config$db$password
)

# disconnect --------------------------------------------------------------
shiny::onStop(function() {
  DBI::dbDisconnect(conn)
})


# setup powpolished -------------------------------------------------------

global_sessions_config(
  api_url = app_config$powpolished$api_url,
  app_name = app_config$app_name,
  api_key = app_config$powpolished$api_key,
  firebase_config = list(
    apiKey = app_config$firebase$api_key,
    authDomain = app_config$firebase$auth_domain,
    projectId = app_config$firebase$project_id
  ),
  sign_in_providers = c('email', 'phone', 'google')
)

# assets ---------------------------------------

assets <- yaml::read_yaml("config/assets.yml")
pow_colors <- assets$colors

# choices -----------------------------------------------------------------

choices <- yaml::read_yaml("config/choices.yml") %>%
  purrr::map(unlist)


# deprecated --------------------------------------------------------------

# library(dbx)
# library(polished)
# library(tinytex)
# library(shinylogs)
# library(shinytest)
# library(logger)
# library(markdown)
# library(highcharter)

# NOTE
# For config.yml source R/get_config.R and run get_config() OR pull directly from
# GCP secret manager via dev/gcloud_secret.(ps1|.sh)

# source("R/docker.R")
