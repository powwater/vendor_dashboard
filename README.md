<p align="center">
<a href="https://powwater.org"><img align="center" src="https://github.com/powwater/adminportal/blob/main/docs/images/powwater_logo_transparent.png"/></a>
</p>

# Powwater Vendors Dashboard 

> Repository for the [Vendors Shiny Web Application Dashboard](https://vendorsdashboard.powwater.org/)


## Deployment Instructions

Step 1: build Docker image

```
# terminal

# build the docker image
docker build -t vendor_dashboard .

# run the docker container.  This is useful for local testing.
docker run --env SHINY_LOG_STDERR=1 --rm -p 8080:8080 vendor_dashboard
```

Step 2: push to Google Container Registry

```
# terminal
# tag image for deployment to GCR (Google Container Registry)
docker tag vendor_dashboard gcr.io/powwater/vendor_dashboard

# push tagged image to GCR
docker push gcr.io/powwater/vendor_dashboard
```

Step 3: open Cloud Run in GCP web console and deploy above image


### Project Management

- [Powwater Organization Home Page](https://github.com/powwater/)
- [Shared Google Drive](https://drive.google.com/drive/folders/1phWTOrfO89lJftvpnFn3jnHHdZwxHM7L?usp=sharing)
- [Slack Channel](powwatertychobra.slack.com)
- [GCP Powwater Project Homepage](https://console.cloud.google.com/home/preview?project=powwater&folder=&organizationId=)
- [GCP Powwater Project Dashboard](https://console.cloud.google.com/home/dashboard?folder=&organizationId=&project=powwater)

#### Data and Documentation 📚

- [MVP Requirements Google Sheet](https://docs.google.com/spreadsheets/d/19O21Ktt42gg5vL0W89xbaEg2cB3zLqlX66JCC_FtWJ4/edit#gid=615292954)
- [Powwater Database Documentation](https://dbdiagram.io/d/603d49dafcdcb6230b220f12)



#### Google Services 💰

- [Google Domains](https://domains.google/)
- [Cloud Console](https://cloud.google.com/)
- [Google Cloud Run](https://console.cloud.google.com/run/&project=powwater)
- [Google Cloud SQL](https://console.cloud.google.com/sql/instances/powwater/overview?authuser=1&project=powwater)
- [Google Cloud Storage](https://console.cloud.google.com/storage?authuser=1&project=powwater)
- [Google Container Registry](https://console.cloud.google.com/gcr/images/powwater?project=powwater&authuser=1)

##### Maps 🌍

- [Google Maps APIs]()
- [Custom Google Map](https://www.google.com/maps/d/u/1/edit?mid=1vwGYrb177d7Nha_MD-seRWRZpqe4_tAi&ll=-4.031851621222264%2C39.676418149999996&z=12)

### Developer Tools 🛠️

- [Docker](https://www.docker.com/)
- [Google Container Registry](https://github.com/orgs/powwater/packages)

### External APIs

- [MPesa](https://www.worldremit.com/en/m-pesa-mobile-wallets?ds_kid=43700022355252936&utm_content=SomalilandAccount&utm_source=bing&utm_medium=cpc&utm_campaign={CampaignName}&utm_term=mpesa|e&gclid=8d9bdc280d65107f3036bc7b84554c20&gclsrc=3p.ds&ds_rl=1262317&ds_rl=1262371&msclkid=8d9bdc280d65107f3036bc7b84554c20)
- [Africas Talking](https://account.africastalking.com/apps/pwzmptvmrj/product-requests/create)
