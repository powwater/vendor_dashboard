# Powwater Vendor Dashboard 

A web application for POW vendors to manage their business operations with POW.  This app allows vendors to manage and monitor their POW customers, riders, orders, etc.

[Live App](https://vendor.powwater.org/)

## Deployment Instructions

Step 1: build Docker image

```
# terminal

# build the docker image
docker build -t pow-vendor .

# run the docker container.  This is useful for local testing.
docker run --env SHINY_LOG_STDERR=1 --rm -p 8080:8080 pow-vendor
```

Step 2: push to Google Container Registry

```
# terminal
# tag image for deployment to GCR (Google Container Registry)
docker tag pow-vendor gcr.io/powwater/pow-vendor

# push tagged image to GCR
docker push gcr.io/powwater/pow-vendor
```

Step 3: open Cloud Run in GCP web console and deploy above image
