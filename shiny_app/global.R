# library packages --------------------------------------------------------
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dbplyr)
  library(dplyr)
  library(DT)
  library(formattable)
  library(polished)
  library(snakecase)
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
  library(tidyselect)
  library(attempt)
  library(googleway)
  library(shinyFiles)
  library(waiter)
  library(shinyscroll)
  library(dialr)
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



# load config yaml file
app_config <- config::get()

# setup google maps API key for googleway
key <- app_config$gcp$gmaps_api_key
set_key(key = key)


conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = app_config$db$host,
  dbname = app_config$db$dbname,
  user = app_config$db$user,
  password = app_config$db$password
)

shiny::onStop(function() {
  DBI::dbDisconnect(conn)
})


# setup polished -------------------------------------------------------

global_sessions_config(
  app_name = app_config$app_name,
  api_key = app_config$polished$api_key
)

# assets ---------------------------------------

assets <- yaml::read_yaml("www/assets.yml")
pow_colors <- assets$colors

# choices -----------------------------------------------------------------

choices <- yaml::read_yaml("choices.yml") %>%
  purrr::map(unlist)

