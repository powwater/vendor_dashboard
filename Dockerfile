FROM gcr.io/powwater/shiny_run_server

ARG R_CONFIG_ACTIVE=default

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
  libpq-dev \ 
  libgit2-dev

RUN R -e "install.packages('remotes')"

# CRAN R packages
RUN R -e "remotes::install_version('DBI', version = '1.1.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('RPostgres', version = '1.3.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('dbx', version = '0.2.8', upgrade = 'never')" 
RUN R -e "remotes::install_version('urltools', version = '1.7.3', upgrade = 'never')" 
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
RUN R -e "remotes::install_version('rprojroot', version = '2.0.2', upgrade = 'never')" 
RUN R -e "remotes::install_version('fs', version = '1.5.0', upgrade = 'never')" 
RUN R -e "remotes::install_version('googledrive', version = '1.0.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('usethis', version = '2.0.1', upgrade = 'never')" 
RUN R -e "remotes::install_version('qs', version = '0.24.1', upgrade = 'never')"



# GitHub R packages
RUN R -e "remotes::install_github('powwater/powpolished', ref = '5e38b291fdb2103e8c8247db4811a2b54ea0d13c', upgrade='never')" 
RUN R -e "remotes::install_github('tychobra/tychobratools', ref = 'c1a24b413363174f1115a0b34d9ae3f8616e5b76', upgrade='never')" 
RUN R -e "remotes::install_github('merlinoa/shinyFeedback', ref = 'ceb65e02428181a166a8b2cb20aac727c6f261ab', upgrade='never')" 
RUN R -e "remotes::install_github('JohnCoene/shinyscroll', ref = '98f5d669ab76c9e44fda798beac4c17dd58f66c9', upgrade='never')"



# Copy, Configure, and Run Shiny Server
COPY 'shiny_app' /srv/shiny-server/shiny_app

RUN chown -R shiny:shiny /srv/shiny-server/shiny_app

RUN echo "Sys.setenv(R_CONFIG_ACTIVE='$R_CONFIG_ACTIVE')" >> /home/shiny/.Rprofile

USER shiny

CMD ["/usr/bin/shiny-server"]
