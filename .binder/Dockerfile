FROM rocker/binder:4.3.1
LABEL maintainer='Unknown'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}

RUN wget https://github.com/seabbs/cste-forecasting-workshop/raw/main/renv
RUN wget https://github.com/seabbs/cste-forecasting-workshop/raw/main/renv.lock

RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'
