# deploy to cloud run - no traffic dry run
system(
  paste0(
    "gcloud run deploy powwater --memory=8192Mi --platform=managed --cpu=4 --image=gcr.io/powwater/powwater_vendorsdashboard
  --max-instances='default' min-instances=0 --port=8080 --no-traffic --allow-unauthenticated --region=", `<gcp_region>`, ""
  )
)

# dev deployment (with revision suffix) and tag deployment
system(
  paste0("gcloud run deploy powwater --memory=8192Mi --platform=managed --cpu=4 --image=gcr.io/powwater/powwater_vendorsdashboard",
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
