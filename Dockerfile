FROM rocker/r-ver:4.0.4

EXPOSE 8080

# System Requirements
RUN apt-get update -qq && apt-get -y --no-install-recommends install \ 
  libjpeg-dev \ 
  libcurl4-openssl-dev \ 
  libssl-dev \ 
  make \ 
  default-jdk \ 
  git \ 
  libicu-dev \ 
  pandoc \ 
  libpq-dev

RUN R -e "install.packages('remotes')"

# CRAN R packages 
RUN R -e "remotes::install_version('DBI', version = '1.1.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('RPostgres', version = '1.3.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('dbplyr', version = '2.1.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('dplyr', version = '1.0.5', upgrade = 'never')" 
RUN R -e "remotes::install_version('DT', version = '0.17', upgrade = 'never')" 
RUN R -e "remotes::install_version('formattable', version = '0.2.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('snakecase', version = '0.11.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('htmltools', version = '0.5.1.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('shiny', version = '1.6.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('shinycustomloader', version = '0.9.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('shinydashboard', version = '0.7.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('shinydashboardPlus', version = '0.7.5', upgrade = 'never')" 
RUN R -e "remotes::install_version('shinyjs', version = '2.0.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('shinyWidgets', version = '0.6.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('yaml', version = '2.2.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('lubridate', version = '1.7.10', upgrade = 'never')" 
RUN R -e "remotes::install_version('purrr', version = '0.3.4', upgrade = 'never')" 
RUN R -e "remotes::install_version('rlang', version = '0.4.10', upgrade = 'never')" 
RUN R -e "remotes::install_version('tidyselect', version = '1.1.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('attempt', version = '0.3.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('googleway', version = '2.7.3', upgrade = 'never')" 
RUN R -e "remotes::install_version('shinyFiles', version = '0.9.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('waiter', version = '0.2.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('dialr', version = '0.3.2', upgrade = 'never')" 
RUN R -e "remotes::install_version('config', version = '0.3.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('tibble', version = '3.1.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('stringr', version = '1.4.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('scales', version = '1.1.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('fs', version = '1.5.0', upgrade = 'never')"



# GitHub R packages 
RUN R -e "remotes::install_github('tychobra/polished', ref = 'ca514d8d10c6fb9de03a358ba9381184e5d7f210', upgrade='never')" 
RUN R -e "remotes::install_github('tychobra/tychobratools', ref = 'c1a24b413363174f1115a0b34d9ae3f8616e5b76', upgrade='never')" 
RUN R -e "remotes::install_github('merlinoa/shinyFeedback', ref = 'ceb65e02428181a166a8b2cb20aac727c6f261ab', upgrade='never')" 
RUN R -e "remotes::install_github('JohnCoene/shinyscroll', ref = '98f5d669ab76c9e44fda798beac4c17dd58f66c9', upgrade='never')"



# Copy app into shiny server `shiny_app` directory
COPY 'shiny_app' /srv/shiny-server/shiny_app

CMD ["Rscript","-e","shiny::runApp(appDir='/srv/shiny-server/shiny_app',port=8080,launch.browser=FALSE,host='0.0.0.0')"]
