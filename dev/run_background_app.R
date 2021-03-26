run_app <- function() {
  wd <- rprojroot::find_root(rprojroot::has_file_pattern(".Rproj"))
  rstudioapi::jobRunScript("dev/shiny_run.R", workingDir = wd)
  shiny::onSessionEnded(function() rstudioapi::jobRemove("shiny_run.R"))
}

run_app()
