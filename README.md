<p align="center">
<a href="https://powwater.org"><img align="center" src="https://github.com/powwater/adminportal/blob/main/docs/images/powwater_logo_transparent.png"/></a>
</p>

# Powwater Vendors Dashboard 

> Repository for the [Vendors Shiny Web Application Dashboard](https://vendorsdashboard.powwater.org/) build for [Powwater](https://powwater.com) by [Tychobra](https://tychobra.com)

<!-- badges: start -->
[![Lifecycle:Maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Links 🔗

### Project Management 📋

- [Project Status Board](https://github.com/orgs/Tychobra/projects/3?fullscreen=true)
- [Powwater Organization Home Page](https://github.com/powwater/)
- [Shared Google Drive](https://drive.google.com/drive/folders/1phWTOrfO89lJftvpnFn3jnHHdZwxHM7L?usp=sharing)
- [Slack Channel](powwatertychobra.slack.com)
- [GCP Powwater Project Homepage](https://console.cloud.google.com/home/preview?project=powwater&folder=&organizationId=)
- [GCP Powwater Project Dashboard](https://console.cloud.google.com/home/dashboard?folder=&organizationId=&project=powwater)

#### Data and Documentation 📚

- [MVP Requirements Google Sheet](https://docs.google.com/spreadsheets/d/19O21Ktt42gg5vL0W89xbaEg2cB3zLqlX66JCC_FtWJ4/edit#gid=615292954)
- [Powwater Database Documentation](https://dbdiagram.io/d/603d49dafcdcb6230b220f12)

### Applications 💻

- [Pow Apps Landing Page](https://apps.powwater.org/)
- [Vendors Dashboard Web Application](https://vendorsdashboard.powwater.org/)
- [Atlas Admin Portal Web Application](https://vendorsdashboard.powwater.org/)
- [Powpolished API](https://github.com/powwater/powpolished/)

### Repositories 📦

- [Landing Page Repository]()
- [Powwater Main Repository]()
- [Customer Mobile Application Repository]()
- [Adminportal Repository]()
- [Vendors Dashboard Repository]()

#### Google Services 💰

- [Google Domains](https://domains.google/)
- [Cloud Console](https://cloud.google.com/)
- [Google Cloud Run](https://console.cloud.google.com/run/&project=powwater)
- [Compute Engine](https://console.cloud.google.com/compute?authuser=1&project=powwater)
- [App Engine](https://console.cloud.google.com/appengine?authuser=1&project=powwater)
- [Virtual Machine Engines](https://console.cloud.google.com/gve?authuser=1&project=powwater)
- [Google Cloud Build](https://console.cloud.google.com/cloud-build/dashboard?authuser=1&project=powwater)
- [Google Cloud SQL](https://console.cloud.google.com/sql/instances/powwater/overview?authuser=1&project=powwater)
- [Google Cloud Storage](https://console.cloud.google.com/storage?authuser=1&project=powwater)
- [Google Container Registry](https://console.cloud.google.com/gcr/images/powwater?project=powwater&authuser=1)

##### Maps 🌍

- [Google Maps APIs]()
- [Custom Google Map](https://www.google.com/maps/d/u/1/edit?mid=1vwGYrb177d7Nha_MD-seRWRZpqe4_tAi&ll=-4.031851621222264%2C39.676418149999996&z=12)

##### Authentication and User Management 🔐

- [Firebase](https://firebase.google.com/?authuser=1)
- [Polished](https://polished.tech)
- [Polished Dashboard](https://dashboard.polished.tech/)
- [Powpolished API Dashboard](https://dashboard.powwater.org/)


### Developer Tools 🛠️

- [dbdocs.io](https://dbdocs.io/) and [DBML - Database Markup Language](https://www.dbml.org/home/)
- [PostgreSQL](https://www.postgresql.org/)
- [Expo](https://expo.io/tools) | [Github](https://github.com/expo/expo)
- [Docker](https://www.docker.com/)

- [Google Container Registry]()
- [Github Container Registry](https://github.com/powwater/vendor_dashboard/packages/)

### External APIs

- [FlutterWave Dashboard](https://dashboard.flutterwave.com/dashboard/transactions/all)
- [MPesa](https://www.worldremit.com/en/m-pesa-mobile-wallets?ds_kid=43700022355252936&utm_content=SomalilandAccount&utm_source=bing&utm_medium=cpc&utm_campaign={CampaignName}&utm_term=mpesa|e&gclid=8d9bdc280d65107f3036bc7b84554c20&gclsrc=3p.ds&ds_rl=1262317&ds_rl=1262371&msclkid=8d9bdc280d65107f3036bc7b84554c20)
- [Africas Talking](https://account.africastalking.com/apps/pwzmptvmrj/product-requests/create)
- [Twilio](https://www.twilio.com/console/phone-numbers/verified)

***

## Tech Stack & Resources

- The application runs off of [Shiny Server Open Source](https://rstudio.com/products/shiny/shiny-server/) and is written using the [R Programming Language](https://www.r-project.org/) in conjunction with the [Shiny](https://github.com/rstudio/shiny) R Package.
- In addition, other various R packages are utilized throughout the application along with traditional HTML, CSS, and Javascript.

*For more details on shiny-server, please see the [Shiny Server Open Source Administrators Guide](http://rstudio.github.io/shiny-server/os/0.4.0/).*

- The project is hosted on [Google Cloud Platform](https://console.cloud.google.com/home/dashboard?authuser=1&project=powwater):
 - The application is served via [Google Cloud Run](https://console.cloud.google.com/run/detail/asia-east1/powwater-adminportal/metrics?authuser=1&project=powwater)
 - Note that other Google API’s such as [Compute Engine](https://console.cloud.google.com/compute?authuser=1&project=powwater), [App Engine](https://console.cloud.google.com/appengine?authuser=1&project=powwater), and [Virtual Machine Engines](https://console.cloud.google.com/gve?authuser=1&project=powwater) may also be utilized during the development and hosting of the application.
 - The application’s builds are triggered via [Google Cloud Build](https://console.cloud.google.com/cloud-build/dashboard?authuser=1&project=powwater).
 - Secret’s and configuration files are encrypted and stored using the *Google Secrets API*.
 - The back-end database is served via [PostgreSQL](https://www.postgresql.org/) and hosted on [Google Cloud SQL](https://console.cloud.google.com/sql/instances/powwater/overview?authuser=1&project=powwater).
-  Where necessary, for storage, [Google Cloud Storage](https://console.cloud.google.com/storage?authuser=1&project=powwater) will be used to house local storage files (such as documents or data backups).
-  Deployments are containerized using [Docker](https://www.docker.com/) and pushed/stored on the [Google Container Registry](https://console.cloud.google.com/gcr/images/powwater?project=powwater&authuser=1) as well as [Github Container Registry]().
-  Authentication is served via and alternative, customized version of [Polished](https://polished.tech); [Powpolished]() and is managed through the [Powpolished Dashboard](https://dashboard.powwater.tech/).
- [Firebase](https://firebase.google.com/?authuser=1) is used to allow authentication through external channels such as Microsoft or Google Accounts.
- Custom Domains are managed using [Google Domains](https://domains.google/) specific to Powwater.
- Traffic is monitored and re-directed between deployments automatically through our DevOps pipeline.
- The project is managed and version controlled using Git and hosted in this private Github repository.
- Shared documents and cloud storage can be found on the project’s [Shared Google Drive](https://drive.google.com/drive/folders/1phWTOrfO89lJftvpnFn3jnHHdZwxHM7L?usp=sharing).
- The project also has a dedicated [Slack Channel](https://powwatertychobra.slack.com/archives/C01JDL820MA) setup for communication.

## Contacts 📧

### Powwater

-   Alec Bernstein: <alec@powwater.com>
-   Anne Gitau: <anne@powwater.com>
-   Casey: <casey@powwater.com>
-   Daniel Ogonya: <daniel@powwater.com>
-   Ellie O’Neill: <ellie@powwater.com>
-   Jack Hartpence: <jack@powwater.com>
-   Maria: <maria@powwater.com>

Board of Advisors:

-   Dr. Joshua Cohen, In-House Philosopher, Apple
-   Paul Shang, Vice-Chairman, Standard Bank
-   Sandy Hessler, Fmr. Assistant Dean, Harvard Kennedy
-   Dr. Tony Colman, Fmr. Labour Member of Parliament (MP), Founder of TopShop
-   Mark Sullivan, Fmr. Head of US Secret Service
-   Mary Pang, Global Head of Private Client Practice, Cambridge Associates
-   Todd Moss, Fmr. Chief US Diplomat in West Africa, Executive Director, Energy for Growth
-   Murefu Barasa, Managing Director, EED Advisory
-   Benny Adrion, Michael Fritz, Ajay Paul, Viva Con Agua Co-Founding Team

### Tychobra

-   Andy Merlino <andy.merlino@tychobra.com>
-   Jimmy Briggs <jimmy.briggs@tychobra.com>
-   Patrick Howard <patrick.howard@tychobra.com>

------------------------------------------------------------------------

> 2021 Tychobra/Powwater









