FROM rocker/geospatial:3.5.1
ARG git_branch

LABEL maintainer="Luigi Ranghetti <ranghetti.l@irea.cnr.it>"

# Install external dependencies
RUN apt-get update && apt-get install -y \
        gdal-bin \
        python-gdal \
        aria2 \
        libpython-dev \
        libv8-3.14-dev && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/*

# Install the package
RUN R -e "devtools::install_github('ranghetti/sen2r', ref = '$git_branch', dependencies = TRUE)"

# Install internal dependencies and configure
RUN R -e 'sen2r:::load_binpaths(c("python", "wget", "aria2", "gdal", "sen2cor"))'

# Allow user rstudio to use sen2cor
RUN mkdir /root/sen2cor/2.5/log && \
    chmod -R 0777 /root && \
    chmod -R 0777 /root/sen2cor && \
    chmod -R 0777 /usr/local/lib/R/site-library/sen2r/sen2cor
