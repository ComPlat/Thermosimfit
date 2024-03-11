FROM rocker/shiny:4.3.1

RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    curl \
    vim \
    libcurl4-gnutls-dev \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
ENV SHINY_LOG_STDERR=1

RUN install2.r --error --skipinstalled \
    shiny \
    DT \
    shinydashboard \
    shinyWidgets \
    shinyjs \
    rootSolve \
    ggplot2 \
    patchwork \
    R6 \
    sensitivity \
    openxlsx \
    future \
    promises

COPY ./tsf/ /home/tsf
COPY ./app/ /srv/shiny-server/
COPY ./run.sh .
# docker cp ./app/ 74c5edc0ad6c:/srv/shiny-server/ 

RUN bash -c "cd /home/tsf; R CMD INSTALL ."

USER shiny
EXPOSE 3838

ENV SHINY_LOG_STDERR=1

CMD ["/bin/bash", "-c", "./run.sh"]

