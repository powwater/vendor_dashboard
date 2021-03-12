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

# build production, tag, and push to GCR
system("gcloud auth configure-docker")
system("docker build --build-arg R_CONFIG_ACTIVE=production -t powwater_vendorsdashboard .")
system("docker tag powwater_vendorsdashboard gcr.io/powwater/powwater_vendorsdashboard")
system("docker push gcr.io/powwater/powwater_vendorsdashboard")

# deploy to cloud run - no traffic dry run
system(
  paste0(
    "gcloud run deploy powwater --memory=8192Mi --platform=managed --cpu=4 --image=gcr.io/powwater/powwater_adminportal
  --max-instances='default' min-instances=0 --port=8080 --no-traffic --allow-unauthenticated --region=", `<gcp_region>`, ""
  )
)

# dev deployment (with revision suffix) and tag deployment
system(
  paste0("gcloud run deploy powwater --memory=8192Mi --platform=managed --cpu=4 --image=gcr.io/powwater/powwater_adminportal",
         "--max-instances='default' min-instances=0 --port=8080 --tag", `<deployment_tag>`, "--allow-unauthenticated --revision-suffix=",
         `<revision_suffix>`, "--region=asia-east1 --add-cloudsql-instances powwater:asia-south1:powwater --concurrency=80")
)

deploy_cloud_run_app(
  service_name = 'powwater',
  docker_image = 'gcr.io/powwater/powwater_vendorsdashboard',
  gcp_region = 'asia-east1',
  cloud_sql_instances = 'powwater:asia-south1:powwater',
  min_instances = 0,
  max_instances = 'default',
  tag = "latest",
  revision_suffix = NULL,
  concurrency = 80,
  dry_run = FALSE,
  gcloud = NULL
)
