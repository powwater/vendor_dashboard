
#  ------------------------------------------------------------------------
#
# Title : Global
#    By : Jimmy Briggs
#  Date : 2021-03-09
#
#  ------------------------------------------------------------------------

# NOTE
# For config.yml source R/utils.R or dev/get_config.R and run get_config().

# clear global environment ------------------------------------------------
# rm(list = ls())

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
  waiter
)


# data --------------------------------------------------------------------

# customer_locations <- qs::qread("data/customer_locations")
# vendor_locations <- qs::qread("data/vendor_locations")

# customer_location_details <- qs::qread("data/customer_location_details")

# set default options -----------------------------------------------------
# options(tinytex.verbose = TRUE)
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

# load configuration ------------------------------------------------------

# tell app if in development or not
# is_dev <- Sys.getenv("R_CONFIG_ACTIVE", "default") == "default"

# enable shiny devmode
# if (is_dev && packageVersion("shiny") >= "1.6.0") shiny::devmode()

# load config yaml file
attempt::attempt({
  app_config <- config::get()
}, msg = "Error loading configuration. Run `get_config()` to retrieve config.yml from Google Drive and re-try.")

key <- app_config$gcp$gmaps_api_key
set_key(key = key)

# setup polished ----------------------------------------------------------

powpolished::global_sessions_config(
  api_url = app_config$powpolished$api_url,
  app_name = app_config$powpolished$app_name,
  api_key = app_config$powpolished$api_key,
  sign_in_providers = 'email'
)

# setup database connection -----------------------------------------------

attempt::attempt({
  conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    user = app_config$db$user,
    dbname = app_config$db$dbname,
    password = app_config$db$password,
    host = app_config$db$host
  )
}, msg = "Error connecting to database..")

# assets ---------------------------------------

assets <- yaml::read_yaml("config/assets.yml")
pow_colors <- assets$colors

# choices -----------------------------------------------------------------

# choices <- yaml::read_yaml("config/choices.yml")

# disconnect --------------------------------------------------------------
shiny::onStop({
  function() DBI::dbDisconnect(conn)
})


# utils -------------------------------------------------------------------

get_last_updated_date <- function(path = ".") {
  fs::dir_info(path) %>%
    tibble::as_tibble() %>%
    dplyr::pull("modification_time") %>%
    max(na.rm = TRUE)
}


# choices -----------------------------------------------------------------

choices <- list(
  inventory = list(
    capacity = c(
      "1.5 Liters" = 1.5,
      "10 Liters" = 10,
      "20 Liters" = 20
    ),
    offer_type = c(
      "New", "Swap"
    )
  )
)
