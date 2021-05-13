FROM rocker/r-ver:4.0.5 AS sysreqs

ENV CRAN_REPO https://packagemanager.rstudio.com/cran/__linux__/focal/latest

RUN R -e "install.packages('remotes', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))"

# System Requirements
RUN apt-get update -qq && apt-get -y --no-install-recommends install \ 
  libpq-dev \ 
  pandoc \ 
  libicu-dev \ 
  make \ 
  libjpeg-dev \ 
  libcurl4-openssl-dev \ 
  libssl-dev \ 
  libjq-dev \ 
  default-jdk \ 
 && rm -rf /var/lib/apt/lists/*

FROM sysreqs AS rpackages

COPY .Rprofile.site /usr/lib/R/etc/

RUN R -e "install.packages('remotes', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))"

# CRAN R packages 
RUN R -e "remotes::install_version('DBI', version = '1.1.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('RPostgres', version = '1.3.2', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('dbplyr', version = '2.1.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('dplyr', version = '1.0.6', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('DT', version = '0.18', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('formattable', version = '0.2.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('snakecase', version = '0.11.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('htmltools', version = '0.5.1.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shiny', version = '1.6.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shinycustomloader', version = '0.9.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shinydashboard', version = '0.7.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shinydashboardPlus', version = '0.7.5', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shinyFeedback', version = '0.3.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shinyjs', version = '2.0.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shinyWidgets', version = '0.6.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('yaml', version = '2.2.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('lubridate', version = '1.7.10', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('purrr', version = '0.3.4', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('rlang', version = '0.4.11', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('tidyselect', version = '1.1.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('attempt', version = '0.3.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('googleway', version = '2.7.3', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('shinyFiles', version = '0.9.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('dialr', version = '0.3.2', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('config', version = '0.3.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('tibble', version = '3.1.1', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('stringr', version = '1.4.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))" 
RUN R -e "remotes::install_version('fs', version = '1.5.0', upgrade = 'never', repos = c('CRAN' = Sys.getenv('CRAN_REPO')))"


# GitHub R packages 
RUN R -e "remotes::install_github('powwater/powpolished', ref = 'a02c2459d306d4ef7d5ee10b9509f52c7089234d', upgrade='never')" 
RUN R -e "remotes::install_github('JohnCoene/waiter', ref = '477789d177556f9fd11c9120f5b71b72015706c1', upgrade='never')" 
RUN R -e "remotes::install_github('johncoene/shinyscroll', ref = '98f5d669ab76c9e44fda798beac4c17dd58f66c9', upgrade='never')"


FROM rpackages AS shinyapp

COPY . /srv/shiny-server/shiny_app

EXPOSE 8080

CMD ["Rscript","-e","shiny::runApp(appDir='/srv/shiny-server/shiny_app',port=8080,launch.browser=FALSE,host='0.0.0.0')"]
