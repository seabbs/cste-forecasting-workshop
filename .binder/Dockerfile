FROM rocker/binder:4.3.1
LABEL maintainer='Sam Abbott'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}

# Install system libraries required for renv
USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev

# Switch back to jovyan to install R packages
USER ${NB_USER}

RUN Rscript -e 'renv::restore()'
