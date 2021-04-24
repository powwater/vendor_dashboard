configure_app <- function(docker = FALSE) {

  check_packages()

  # set default options -----------------------------------------------------
  options(shiny.trace = FALSE) # set to T to print full shiny operations.
  options(scipen = 999)
  options(dplyr.summarise.inform = FALSE)
  options(lubridate.week.start = 1)

  # tell app if in development or not
  is_dev <- Sys.getenv("R_CONFIG_ACTIVE", "default") %in% c("default", "local")

  # local
  is_local <- Sys.getenv('SHINY_PORT') == ""

  # download latest config.yml file:
  if (is_local) source(here::here("shiny_app/R/get_config.R")); get_config()

  # docker
  is_docker <- docker_is_running() &&  is_local && docker
  if (is_docker) Sys.setenv("R_CONFIG_ACTIVE" = "local")

  # load configuration ------------------------------------------------------
  app_config <- config::get()

  # run DB container
  if (docker) {
    # determine if pow_dev_db has been built:
    db_built <- "pow_dev_db" %in% docker_images()$repository
    if (!db_built) stop("Build the database image first with tag 'pow_dev_db:public'.")

    db_running <- "pow_dev_db:public" %in% docker_containers()$image
    if (!db_running) {
      # run local docker database for development
      rstudioapi::terminalExecute("docker run -e POSTGRES_PASSWORD=p -it -p 5432:5432 pow_dev_db:public")
    }

  }

  # enable shiny devmode
  if (is_dev && is_local && packageVersion("shiny") >= "1.6.0") shiny::devmode()

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

  return(app_config)

}

check_packages <- function() {
  if (packageVersion("shinydashboardPlus") > "0.7.5") {
    usethis::ui_stop(
      paste0(
        "package `shinydashboardPlus` must use version 0.7.5; run ",
        usethis::ui_code("remotes::install_version('shinydashboardPlus', '0.7.5')"),
        " to install."
      )
    )
  }
}
