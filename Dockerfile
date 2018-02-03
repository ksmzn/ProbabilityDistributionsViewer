FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
  libssl-dev/unstable \
  libssh2-1-dev/unstable \
  build-essential

RUN R -e "install.packages(c('devtools', 'shinydashboard', 'hypergeo', 'purrr', 'R6', 'dplyr', 'stringr'), repos='http://cran.rstudio.com/')"

RUN R -e "devtools::install_github('Appsilon/shiny.i18n')"

CMD ["/usr/bin/shiny-server.sh"]
