
-   [fidolasen: FInd, DOwnload and preprocess LAndsat and SENtinel images](#fidolasen-find-download-and-preprocess-landsat-and-sentinel-images)
    -   [Warning](#warning)
    -   [Installation](#installation)
    -   [Credits](#credits)

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/ggranga/fidolasen.svg?branch=master)](https://travis-ci.org/ggranga/fidolasen) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/fidolasen)](https://cran.r-project.org/package=fidolasen) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

fidolasen: FInd, DOwnload and preprocess LAndsat and SENtinel images
====================================================================

Warning
-------

This package is under construction: for now only the functions documented in the [function references](https://ggranga.github.io/fidolasen/reference/index.html) are working.

Current version is pre-release 0.2.0 (see [release details](https://github.com/ggranga/fidolasen/releases/tag/0.2.0)).

Installation
------------

You can install fidolasen from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ggranga/fidolasen")
```

This will install the R package, containing all the functions necessary to preprocess data. To download Sentinel-2 images and perform atmospheric correction with [sen2cor](http://step.esa.int/main/third-party-plugins-2/sen2cor), the package makes use of a set of Python functions ([s2download](https://github.com/ggranga/s2download)). To import these scripts, run R function [`s2_download()`](https://ggranga.github.io/fidolasen/reference/install_s2download.html) included in the package. Please notice that the use of sen2cor algorythm for now is possible only under Linux systems, and the installation of the docker (necessary to run it) requires some time and a consistent amount of disk space.

Credits
-------

`fidolasen` is being developed by Luigi Ranghetti and Lorenzo Busetto ([IREA-CNR](http://www.irea.cnr.it)), and it is released under [GPL 3.0](https://www.gnu.org/licenses/gpl.html).
