
library(polished)
library(yaml)
library(whisker)
library(usethis)
library(purrr)

source("dev/sysreqs.R")
source("dev/pkgdeps.R")
source("dev/use_template.R")

# create new "build" folder
fs::dir_delete("build")
fs::dir_create("build")
copy_dirs <- c(
  "shiny_app/R",
  "shiny_app/config",
  "shiny_app/www"
)
copy_files <- c(
  "shiny_app/.Rprofile.site",
  "shiny_app/.dockerignore",
  "shiny_app/config.yml",
  fs::dir_ls("shiny_app", type = "file", glob = "*.R")
)

purrr::walk(copy_dirs, ~ fs::dir_copy(.x, paste0("build/", basename(.x)), overwrite = TRUE))
purrr::walk(copy_files, fs::file_copy, "build/", overwrite = TRUE)
# fs::dir_delete("build/R/dev")

# shiny::runApp(appDir = "build")

# gather R package dependencies -------------------------------------------
optional_pkgs <- c("googledrive", "qs", "dbx", "urltools", "rprojroot", "usethis")
deps <- polished:::get_package_deps("shiny_app")
deps <- deps[!(names(deps) %in% c(optional_pkgs, "remotes"))]
yaml::write_yaml(deps, "shiny_app/deps.yml")

# command strings ---------------------------------------------------------
cran_install_cmd <- get_cran_deps(deps) %>% cran_packages_cmd()
gh_install_cmd <- get_gh_deps(deps) %>% gh_packages_cmd()
sysreqs_cmd <- get_sysreqs_pak(names(deps)) %>% sysreqs_cmd()

# create Dockerfile from template -----------------------------------------
use_template("dev/templates/Dockerfile_template",
             "shiny_app/Dockerfile",
             data = list(
               sysreqs = sysreqs_cmd,
               cran_installs = cran_install_cmd,
               gh_installs = gh_install_cmd
             ))

# create .dockerignore ----------------------------------------------------
# write("shiny_app/.dockerignore", ".dockerignore")
# write("shiny_app/Dockerfile", ".dockerignore", append = TRUE)
# write("shiny_app/logs/*", ".dockerignore", append = TRUE)
# write("shiny_app/deps.yaml", ".dockerignore", append = TRUE)
# write("shiny_app/README.md", ".dockerignore", append = TRUE)
# write("shiny_app/R/get_config.R", ".dockerignore", append = TRUE)

# start docker, build local test image and run
shell.exec("C:/Program Files/Docker/Docker/Docker Desktop.exe")
rstudioapi::terminalExecute("docker build -t powwater_vendorsdashboard .")
rstudioapi::terminalExecute("docker run --env SHINY_LOG_STDERR=1 --rm -p 8080:8080 powwater_vendorsdashboard")
browseURL("localhost:8080")

# build production, tag, and push to GCR
rstudioapi::terminalExecute("gcloud auth configure-docker")
rstudioapi::terminalExecute("docker build --build-arg R_CONFIG_ACTIVE=production -t powwater_vendorsdashboard .")
rstudioapi::terminalExecute("docker tag powwater_vendor_dashboard gcr.io/powwater/powwater_vendor_dashboard")
rstudioapi::terminalExecute("docker push gcr.io/powwater/powwater_vendorsdashboard")

# deploy to cloud run
system("gcloud run deploy powwater-vendorsdashboard --memory=8192Mi --platform=managed --cpu=4 --image=gcr.io/powwater/powwater_vendorsdashboard --max-instances='default' --min-instances=0 --port=8080 --no-traffic --allow-unauthenticated --region=asia-east1")

# open cloud run
browseURL("https://console.cloud.google.com/run/detail/asia-east1/powwater-vendorsdashboard/revisions?project=powwater")

# open app
browseURL("https://vendorsdashboard.powwater.org")

# Only run if want to push to container registries outside of GCR: --------

# tag and push to github
# system("cat /mnt/c/Users/jimbrig/.docker/github_PAT.txt | docker login https://docker.pkg.github.com -u jimbrig --password-stdin")
# system("docker tag powwater_vendorsdashboard docker.pkg.github.com/powwater/vendor_dashboard/powwater_vendorsdashboard:latest")
# system("docker build -t docker.pkg.github.com/powwater/vendor_dashboard/powwater_vendorsdashboard:latest .")
# system("docker push docker.pkg.github.com/powwater/vendor_dashboard/powwater_vendorsdashboard:latest")

# tag and push to dockerhub
# system("docker tag powwater_vendorsdashboard jimbrig2011/powwater_vendorsdashboard:alpha")
# system("docker build -t jimbrig2011/powwater_vendor_dashboard:alpha .")
# system("docker push jimbrig2011/powwater_vendor_dashboard:alpha")
