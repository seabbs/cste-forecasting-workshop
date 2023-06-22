FROM ghcr.io/rocker-org/devcontainer/tidyverse:4

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-dev

USER rstudio 

COPY renv renv
COPY renv.lock renv.lock

RUN Rscript -e 'renv::restore()'
