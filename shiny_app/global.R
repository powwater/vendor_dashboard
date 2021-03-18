
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
  shinyscroll
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
}, msg = "Error loading configuration. Run `get_config()` to retrieve config.yml from Google Drive and re-try.")

key <- app_config$gcp$gmaps_api_key
set_key(key = key)

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

# data --------------------------------------------------------------------

customers_query <- conn %>%
  dplyr::tbl("orders") %>%
  dplyr::distinct(customer_uid, customer_name) %>%
  dplyr::left_join(
    conn %>% dplyr::tbl("customer_locations") %>%
      dplyr::select(-c(created_at:modified_by)) %>%
      rename(customer_location_uid = uid),
    by = c("customer_uid")
  ) %>%
  dplyr::left_join(
    conn %>% dplyr::tbl("customers"),
    by = c("customer_uid" = "uid", "customer_name")
  ) %>%
  dplyr::rename(customer_location_url = customer_location_url.x, customer_location_url_full = customer_location_url.y) %>%
  dplyr::select(-customer_location) %>%
  left_join(
    conn %>% dplyr::tbl("order_routes") %>% dplyr::select(-uid, -order_uid, -c(created_at:modified_by)),
    by = c("customer_location_uid")
  ) %>%
  left_join(
    conn %>% dplyr::tbl("vendor_locations") %>% dplyr::select(-c(created_at:modified_by)),
    by = c("vendor_location_uid" = "uid")
  )

orders_query <- conn %>%
  dplyr::tbl("orders") %>%
  rename(order_uid = uid)

# setup polished ----------------------------------------------------------

powpolished::global_sessions_config(
  api_url = app_config$powpolished$api_url,
  app_name = app_config$powpolished$app_name,
  api_key = app_config$powpolished$api_key,
  sign_in_providers = 'email'
)

# assets ---------------------------------------

assets <- yaml::read_yaml("config/assets.yml")
pow_colors <- assets$colors

# choices -----------------------------------------------------------------
choices <- yaml::read_yaml("config/choices.yml") %>%
  purrr::map(unlist)

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
