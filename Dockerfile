FROM rocker/r-ver:4.1.0

#ENV CRAN_REPO https://packagemanager.rstudio.com/cran/__linux__/focal/latest

EXPOSE 8080

# System Requirements
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  make \
  libicu-dev \
  libpq-dev \
  pandoc \
  libjpeg-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libjq-dev \
  default-jdk \
 && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('remotes')"

# CRAN R packages
RUN R -e "remotes::install_version('attempt', version = '0.3.1', upgrade = 'never')"
RUN R -e "remotes::install_version('config', version = '0.3.1', upgrade = 'never')"
RUN R -e "remotes::install_version('DBI', version = '1.1.1', upgrade = 'never')"
RUN R -e "remotes::install_version('dbplyr', version = '2.1.1', upgrade = 'never')"
RUN R -e "remotes::install_version('dialr', version = '0.3.2', upgrade = 'never')"
RUN R -e "remotes::install_version('dplyr', version = '1.0.7', upgrade = 'never')"
RUN R -e "remotes::install_version('DT', version = '0.18', upgrade = 'never')"
RUN R -e "remotes::install_version('formattable', version = '0.2.1', upgrade = 'never')"
RUN R -e "remotes::install_version('fs', version = '1.5.0', upgrade = 'never')"
RUN R -e "remotes::install_version('glue', version = '1.4.2', upgrade = 'never')"
RUN R -e "remotes::install_version('googleway', version = '2.7.3', upgrade = 'never')"
RUN R -e "remotes::install_version('here', version = '1.0.1', upgrade = 'never')"
RUN R -e "remotes::install_version('lubridate', version = '1.7.10', upgrade = 'never')"
RUN R -e "remotes::install_version('purrr', version = '0.3.4', upgrade = 'never')"
RUN R -e "remotes::install_version('readr', version = '1.4.0', upgrade = 'never')"
RUN R -e "remotes::install_version('RPostgres', version = '1.3.2', upgrade = 'never')"
RUN R -e "remotes::install_version('scales', version = '1.1.1', upgrade = 'never')"
RUN R -e "remotes::install_version('shinyFiles', version = '0.9.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinycustomloader', version = '0.9.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinydashboard', version = '0.7.1', upgrade = 'never')"
RUN R -e "remotes::install_version('shinydashboardPlus', version = '0.7.5', upgrade = 'never')"
RUN R -e "remotes::install_version('shinyFeedback', version = '0.3.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinyjs', version = '2.0.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinyWidgets', version = '0.6.0', upgrade = 'never')"
RUN R -e "remotes::install_version('snakecase', version = '0.11.0', upgrade = 'never')"
RUN R -e "remotes::install_version('stringr', version = '1.4.0', upgrade = 'never')"
RUN R -e "remotes::install_version('tibble', version = '3.1.1', upgrade = 'never')"
RUN R -e "remotes::install_version('tidyselect', version = '1.1.1', upgrade = 'never')"

# GitHub R packages
RUN R -e "remotes::install_github('tychobra/polished', ref = 'e9edf53bd269c2477f2fe2e9c19a4991aafd3b3e', upgrade='never')"
RUN R -e "remotes::install_github('tychobra/tychobratools', ref = 'c1a24b413363174f1115a0b34d9ae3f8616e5b76', upgrade='never')"
RUN R -e "remotes::install_github('JohnCoene/waiter', ref = '8361840bf04aa56c72531b07f8bef6909505c5df', upgrade='never')"
RUN R -e "remotes::install_github('johncoene/shinyscroll', ref = '98f5d669ab76c9e44fda798beac4c17dd58f66c9', upgrade='never')"
RUN R -e "remotes::install_github('r-lib/rlang', ref = '7797cdf422910a4c737263f42bd3ea20e6bafcaa', upgrade='never')"
RUN R -e "remotes::install_github('rstudio/htmltools', ref = '7fbab163b0badb7f48c2bbafb56a2886fc951976', upgrade='never')"
RUN R -e "remotes::install_github('rstudio/shiny', ref = '6405056c92713c2b2875c0928964c9ad72999a80', upgrade='never')"




COPY shiny_app /srv/shiny-server/shiny_app

CMD ["Rscript","-e","shiny::runApp(appDir='/srv/shiny-server/shiny_app',port=8080,launch.browser=FALSE,host='0.0.0.0')"]
