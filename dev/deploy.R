
library(polished)
library(yaml)
library(whisker)
library(usethis)
library(purrr)

source("dev/sysreqs.R")
source("dev/pkgdeps.R")
source("dev/use_template.R")

# gather R package dependencies -------------------------------------------
optional_pkgs <- c("googledrive", "qs", "dbx", "urltools", "rprojroot", "usethis")
deps <- polished:::get_package_deps("shiny_app")
deps <- deps[!(names(deps) %in% c(optional_pkgs, "remotes"))]
yaml::write_yaml(deps, "shiny_app/deps.yml")

# gather sysreqs ----------------------------------------------------------
sysreqs <- get_sysreqs(names(deps))

# command strings ---------------------------------------------------------
cran_install_cmd <- get_cran_deps(deps) %>% cran_packages_cmd()
gh_install_cmd <- get_gh_deps(deps) %>% gh_packages_cmd()
sysreqs_cmd <- paste(paste0("RUN ", apt_get_install(sysreqs), collapse = " \\ \n"))

# create Dockerfile from template -----------------------------------------
use_template("dev/Dockerfile_template",
             "Dockerfile",
             data = list(
               sysreqs = sysreqs_cmd,
               cran_installs = cran_install_cmd,
               gh_installs = gh_install_cmd
             ))

# create .dockerignore ----------------------------------------------------
write("shiny_app/logs/*", ".dockerignore")
write("shiny_app/deps.yaml", ".dockerignore", append = TRUE)
write("shiny_app/README.md", ".dockerignore", append = TRUE)
write("shiny_app/R/get_config.R", ".dockerignore", append = TRUE)

# start docker, build local test image and run
shell.exec("C:/Program Files/Docker/Docker/Docker Desktop.exe")
rstudioapi::terminalExecute("docker build -t powwater_vendorsdashboard .")
rstudioapi::terminalExecute("docker run --env SHINY_LOG_STDERR=1 --rm -p 8080:8080 powwater_vendorsdashboard")
browseURL("localhost:8080")

# build production, tag, and push to GCR
rstudioapi::terminalExecute("gcloud auth configure-docker")
rstudioapi::terminalExecute("docker build --build-arg R_CONFIG_ACTIVE=production -t powwater_vendorsdashboard .")
rstudioapi::terminalExecute("docker tag powwater_vendorsdashboard gcr.io/powwater/powwater_vendorsdashboard")
rstudioapi::terminalExecute("docker push gcr.io/powwater/powwater_vendorsdashboard")

# open cloud run
browseURL("https://console.cloud.google.com/run/detail/asia-east1/powwater-vendorsdashboard/revisions?project=powwater")

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
