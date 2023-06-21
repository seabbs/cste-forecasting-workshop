FROM rocker/binder:4.3.1
LABEL maintainer='Unknown'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}

RUN wget https://github.com/seabbs/cste-forecasting-workshop/raw/main/renv
RUN wget https://github.com/seabbs/cste-forecasting-workshop/raw/main/renv.lock

# Install system libraries required for renv
USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev

# Switch back to jovyan to install R packages
USER ${NB_USER}

RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'
