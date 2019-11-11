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
RUN R -e "remotes::install_github('ranghetti/sen2r', ref = 'devel', dependencies = TRUE)"

# Install Sen2Cor
RUN mkdir /opt/sen2cor && \
    mkdir /opt/sen2cor/sen2cor_255 && \
    R -e 'sen2r::install_sen2cor("/opt/sen2cor/sen2cor_255", version = "2.5.5")'

# Download vector of S2 tiles
RUN R -e 'tmp <- sen2r::s2_tiles()'

# Create the user
USER $NB_USER

# Commands to run before opening the container
CMD R -e 'sen2r:::give_write_permission(agree = TRUE)'
CMD R -e 'sen2r::load_binpaths(c("python", "aria2", "gdal"))'
CMD R -e 'sen2r::link_sen2cor("/opt/sen2cor/sen2cor_255")'
