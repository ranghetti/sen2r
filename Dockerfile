FROM rocker/geospatial:3.5.1

LABEL maintainer="Luigi Ranghetti <luigi@ranghetti.info>"

# Install external dependencies
RUN apt-get update && apt-get install -y \
        gdal-bin \
        python-gdal \
        aria2 \
        libpython-dev \
        libv8-3.14-dev && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/*

# Install lwgeom from remote (https://github.com/rocker-org/geospatial/issues/17)
RUN R -e "remotes::install_github('r-spatial/lwgeom', dependencies = TRUE)"

# Install the package
RUN R -e "remotes::install_github('ranghetti/sen2r', ref = 'master', dependencies = TRUE)"

# Create the user's files and settings, set runtime dependencies
RUN sudo -u rstudio mkdir /home/rstudio/.sen2r && \
    sudo -u rstudio R -e 'sen2r::load_binpaths(c("python", "aria2", "gdal"))' && \
    sudo -u rstudio mkdir /home/rstudio/sen2cor && \
    sudo -u rstudio mkdir /home/rstudio/sen2cor/sen2cor_255 && \
    sudo -u rstudio R -e 'sen2r::install_sen2cor("/home/rstudio/sen2cor/sen2cor_255", version = "2.5.5")' && \
    sudo -u rstudio R -e 'tmp <- sen2r::s2_tiles()'
