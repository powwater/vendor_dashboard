FROM rocker/r-ver:4.1.1

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
RUN R -e "remotes::install_version('config', version = '0.3.1', upgrade = 'never')"
RUN R -e "remotes::install_version('DBI', version = '1.1.1', upgrade = 'never')"
RUN R -e "remotes::install_version('dbplyr', version = '2.1.1', upgrade = 'never')"
RUN R -e "remotes::install_version('digest', version = '0.6.27', upgrade = 'never')"
RUN R -e "remotes::install_version('dplyr', version = '1.0.7', upgrade = 'never')"
RUN R -e "remotes::install_version('DT', version = '0.18', upgrade = 'never')"
RUN R -e "remotes::install_version('formattable', version = '0.2.1', upgrade = 'never')"
RUN R -e "remotes::install_version('fs', version = '1.5.0', upgrade = 'never')"
RUN R -e "remotes::install_version('googleway', version = '2.7.3', upgrade = 'never')"
RUN R -e "remotes::install_version('lubridate', version = '1.7.10', upgrade = 'never')"
RUN R -e "remotes::install_version('purrr', version = '0.3.4', upgrade = 'never')"
RUN R -e "remotes::install_version('RPostgres', version = '1.3.3', upgrade = 'never')"
RUN R -e "remotes::install_version('shiny', version = '1.6.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinycssloaders', version = '1.0.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinydashboard', version = '0.7.1', upgrade = 'never')"
RUN R -e "remotes::install_version('shinydashboardPlus', version = '0.7.5', upgrade = 'never')"
RUN R -e "remotes::install_version('shinyFiles', version = '0.9.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinyjs', version = '2.0.0', upgrade = 'never')"
RUN R -e "remotes::install_version('shinyWidgets', version = '0.6.0', upgrade = 'never')"
RUN R -e "remotes::install_version('snakecase', version = '0.11.0', upgrade = 'never')"
RUN R -e "remotes::install_version('stringr', version = '1.4.0', upgrade = 'never')"
RUN R -e "remotes::install_version('tibble', version = '3.1.3', upgrade = 'never')"
RUN R -e "remotes::install_version('tidyselect', version = '1.1.1', upgrade = 'never')"
RUN R -e "remotes::install_version('waiter', version = '0.2.3', upgrade = 'never')"
RUN R -e "remotes::install_version('yaml', version = '2.2.1', upgrade = 'never')"


# GitHub R packages
RUN R -e "remotes::install_github('rstudio/htmltools', ref = 'bc226a3d728c84c53c69fe5d48140fc664fe3dc8', upgrade='never')"
RUN R -e "remotes::install_github('tychobra/polished', ref = '339b4e1c8e52e4409eaee468db1d2b0768e89178', upgrade='never')"
RUN R -e "remotes::install_github('merlinoa/shinyFeedback', ref = 'a9bde3a214007a19d287d0f2aee59371b3a558e7', upgrade='never')"
RUN R -e "remotes::install_github('JohnCoene/shinyscroll', ref = '98f5d669ab76c9e44fda798beac4c17dd58f66c9', upgrade='never')"




COPY shiny_app /srv/shiny-server/shiny_app

CMD ["Rscript","-e","shiny::runApp(appDir='/srv/shiny-server/shiny_app',port=8080,launch.browser=FALSE,host='0.0.0.0')"]
