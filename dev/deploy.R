library(polished)
library(polishedapi)
library(yaml)

write("shiny_app/logs/*", ".dockerignore")
write("shiny_app/deps.yaml", ".dockerignore", append = TRUE)
write("shiny_app/README.md", ".dockerignore", append = TRUE)

deps <- polished:::get_package_deps("shiny_app")
yaml::write_yaml(deps, "shiny_app/deps.yml")
polishedapi:::create_dockerfile("shiny_app/deps.yml", app_dir = "shiny_app")

# start docker, build local test image and run
shell.exec("C:/Program Files/Docker/Docker/Docker Desktop.exe")
system("docker build -t powwater_vendorsdashboard .")
system("docker run --env SHINY_LOG_STDERR=1 --rm -p 8080:8080 powwater_vendorsdashboard")
browseURL("localhost:8080")

# tag and push to github dockerhub
system("cat ~/.docker/github_PAT.txt | docker login https://docker.pkg.github.com -u USERNAME --password-stdin")
system("docker tag powwater_vendorsdashboard docker.pkg.github.com/powwater/vendor_dashboard/powwater_vendorsdashboard:alpha")
system("docker build -t docker.pkg.github.com/powwater/vendor_dashboard/powwater_vendorsdashboard:alpha .")
system("docker push docker.pkg.github.com/powwater/vendor_dashboard/powwater_vendorsdashboard:alpha")

# tag and push to dockerhub
system("docker tag powwater_vendorsdashboard jimbrig2011/powwater_vendorsdashboard:alpha")
system("docker build -t jimbrig2011/powwater_vendor_dashboard:alpha .")
system("docker push")

# build production, tag, and push to GCR
system("gcloud auth configure-docker")
system("docker build --build-arg R_CONFIG_ACTIVE=production -t powwater_vendorsdashboard .")
system("docker tag powwater_vendorsdashboard gcr.io/powwater/powwater_vendorsdashboard")
system("docker push gcr.io/powwater/powwater_vendorsdashboard")

