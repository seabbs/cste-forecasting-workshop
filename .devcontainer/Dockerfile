FROM ghcr.io/rocker-org/devcontainer/tidyverse:4

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev

RUN Rscript -e 'renv::restore()'