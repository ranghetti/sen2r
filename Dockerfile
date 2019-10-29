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

# Install internal dependencies and configure
RUN R -e 'load_binpaths(c("python", "aria2", "gdal"))'
RUN mkdir /root/sen2cor && \
    mkdir /root/sen2cor/sen2cor_255 && \
    R -e 'install_sen2cor("/root/sen2cor/sen2cor_255", version = "2.5.5")'

# Download vector of S2 tiles
RUN R -e 'tmp <- sen2r::s2_tiles()'

# Allow user rstudio to use Sen2Cor
RUN mkdir /root/sen2cor/2.5/log && \
    chmod -R 0777 /root
