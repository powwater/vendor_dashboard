# Use this script to run the shiny app as a local background job and launch in
# browser. This way you can develop the application and view it at the same
# time without having to re-launch browser tabs.

# create the job via Jobs Pane > Start Local Job > Select this script
# to call this script programatically run `rstudioapi::jobRunScript("dev/shiny_run.R")`
options(shiny.launch.browser = TRUE)
options(shiny.autoreload = TRUE)
shiny::runApp("shiny_app")
