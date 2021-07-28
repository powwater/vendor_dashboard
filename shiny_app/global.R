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
  library(shinycssloaders)
  library(shinydashboard)
  library(shinydashboardPlus) # v0.7.5!
  library(shinyFeedback)
  library(shinyjs)
  library(shinyWidgets)
  library(lubridate)
  library(purrr)
  library(tidyselect)
  library(googleway)
  library(shinyFiles)
  library(waiter)
  library(shinyscroll)
  library(dialr)
})

if (packageVersion("shinydashboardPlus") > "0.7.5") {
  stop("package `shinydashboardPlus` must use version 0.7.5")
}

# set default options -----------------------------------------------------
options(shiny.trace = FALSE) # set to T to print full shiny operations.
# turn off scientific notation
options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
options(lubridate.week.start = 1)
# set `shinycssloaders` spinner type
options(spinner.type = 8)


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

choices <- list(
  currency = c(
    "Kenyan Shillings (KES)" = "kes",
    "US Dollars (USD)" = "usd"
  ),
  timezone = c(
    "Auto-Detect" = "auto",
    "East Africa Time (EAT)" = "UTC +3",
    "Eastern Daylight Time (EDT)" = "UTC -4",
    "Central Daylight Time (CDT)" = "UTC -5",
    "Mountain Daylight Time (MDT)" = "UTC -6",
    "Pacific Daylight Time (PDT)" = "UTC -7",
    "Mountain Standard Time (MST)" = "UTC -7"
  ),
  payment_types = c(
    "MPesa" = "mpesa",
    "Credit Card" = "credit_card",
    "Paypal" = "paypal"
  ),
  inventory_capacity = c(
    "10L" = 10,
    "18.9L" = 18.9,
    "20L" = 20.0
  ),
  inventory_offer_type = c(
    "New",
    "Swap",
    "Refill",
    "Dispenser Bottle",
    "PET Bottle",
    "Jerrycan Bottle"
  ),
  order_status = c(
    "Completed" = "Completed",
    "In Progress" = "In Progress",
    "Pending" = "Pending",
    "Canceled" = "Canceled",
    "Rejected" = "Rejected"
  ),
  order_delivery_status = c(
    "Pending" = "Pending",
    "Awaiting Delivery" = "Awaiting Delivery",
    "Out for Delivery" = "Out for Delivery",
    "Delivered" = "Delivered",
    "Canceled" = "Canceled",
    "Rejected" = "Rejected"
  ),
  order_payment_status = c(
    "Pending" = "payment_pending",
    "Confirmed" = "awaiting_vendor_response",
    "Declined" = "declined"
  ),
  vendor_response = c(
    "Accepted" = "Accepted",
    "Rejected" = "Rejected",
    "Pending" = "Pending"
  ),
  phone_number_format = c(
    "National" = "national",
    "International" = "international"
  )
)

