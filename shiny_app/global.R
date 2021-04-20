
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

})

source("R/configure_app.R")
source("R/docker.R")
app_config <- configure_app(docker = TRUE)

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

# assets ---------------------------------------

assets <- yaml::read_yaml("config/assets.yml")
pow_colors <- assets$colors

# choices -----------------------------------------------------------------

choices <- yaml::read_yaml("config/choices.yml") %>%
  purrr::map(unlist)

# setup google maps API key for googleway
key <- app_config$gcp$gmaps_api_key
set_key(key = key)

# deprecated --------------------------------------------------------------

# library(polished)
# library(dbx)
# library(logger)
# library(markdown)
# library(tinytex)
# library(shinylogs)
# library(shinytest)
# library(highcharter)

