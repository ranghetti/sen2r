FROM rocker/geospatial:latest

LABEL maintainer="Luigi Ranghetti <sen2r@ranghetti.info>"

# Install external dependencies
RUN apt update && apt install -y \
        gdal-bin \
        aria2 \
        libpython2-dev && \
    apt autoremove -y && \
    rm -rf /var/lib/apt/lists/*

# Install the package
RUN R -e "remotes::install_github('ranghetti/sen2r', ref = 'main', dependencies = TRUE)"

# Create the user's files and settings, set runtime dependencies
RUN sudo -u rstudio mkdir /home/rstudio/.sen2r && \
    sudo -u rstudio R -e 'sen2r::load_binpaths(c("aria2", "gdal"))' && \
    sudo -u rstudio mkdir /home/rstudio/sen2cor && \
    sudo -u rstudio mkdir /home/rstudio/sen2cor/sen2cor_255 && \
    sudo -u rstudio R -e 'sen2r::install_sen2cor("/home/rstudio/sen2cor/sen2cor_255", version = "2.5.5")' && \
    sudo -u rstudio R -e 'tmp <- sen2r::s2_tiles()'
