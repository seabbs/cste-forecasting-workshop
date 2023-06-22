FROM ghcr.io/rocker-org/devcontainer/tidyverse:4

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-dev

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

USER vscode

COPY renv renv
COPY renv.lock renv.lock

RUN Rscript -e 'renv::restore()'
