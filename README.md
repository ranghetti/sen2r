
-   [fidolasen: FInd, DOwnload and preprocess LAndsat and SENtinel images](#fidolasen-find-download-and-preprocess-landsat-and-sentinel-images)
    -   [Warning](#warning)
    -   [Installation](#installation)
    -   [Usage](#usage)
    -   [Credits](#credits)

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/ranghetti/fidolasen.svg?branch=master)](https://travis-ci.org/ranghetti/fidolasen) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/fidolasen)](https://cran.r-project.org/package=fidolasen) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

fidolasen: FInd, DOwnload and preprocess LAndsat and SENtinel images
====================================================================

Warning
-------

This package is under construction (current version is [pre-release 0.3.1](https://github.com/ranghetti/fidolasen/releases/tag/v0.3.1)): currently only the main processing chain for Sentinel-2 data was implemented. Please refer to the [crontab of release 0.4.0](https://github.com/ranghetti/fidolasen/milestone/3) to know which implementations will be performed regarding Sentinel-2 data. The implementation of the processing chaing over Landsat data is scheduled for the [release 0.5.0](https://github.com/ranghetti/fidolasen/milestone/2).

Installation
------------

You can install **fidolasen** from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ranghetti/fidolasen")
```

This will install the R package, containing all the functions necessary to preprocess data.

To download Sentinel-2 images the package makes use of a set of Python functions ([s2download](https://github.com/ranghetti/s2download)). These scripts are imported the first time hey are used (alternatively, launch the function [`s2_download()`](reference/install_s2download.md) included in the package).

Atmospheric correction is performed using [sen2cor](http://step.esa.int/main/third-party-plugins-2/sen2cor): the package will automatically download and install it at first use, or by running function [`install_sen2cor()`](reference/install_sen2cor.md). Please notice that the use of sen2cor algorythm was not yet possible under MAC.

Preprocessing functions make use of [GDAL](http://www.gdal.org), which must support JPEG2000 format. On Windows, it is strongly recommended to install it using the [OSGeo4W installer](http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86_64.exe) in advanced mode, and checking the installation of `openjpeg` library.

Usage
-----

The simpler way to use **fidolasen** is to execute the function `fidolasen_s2()` without any argument: this opens the GUI to select the processing parameters, and then launches the main function.

Alternatively, [`fidolasen_s2()`](reference/fidolasen_s2.md) can be launched with a list of parameters (created with [`s2_gui()`](reference/s2_gui.md)) or passing manually the parameters as arguments of the function (see the documentation of the function for further details).

Other specific functions can be used to run single steps separately:

-   [`s2_list()`](reference/s2_list.md) to retrieve the list of available Sentinel-2 products basing on input parameters;
-   [`s2_download()`](reference/s2_download.md) to download Sentinel-2 products;
-   [`sen2cor()`](reference/sen2cor.html) to correct level-1C products using [sen2cor](http://step.esa.int/main/third-party-plugins-2/sen2cor);
-   [`s2_translate()`](reference/s2_translate.md) to convert Sentinel-2 products from SAFE format to a format managed by GDAL;
-   [`s2_merge()`](reference/s2_merge.md) to merge Sentinel-2 tiles which have the same date and orbit;
-   [`gdal_warp()`](reference/gdal_warp.md) to clip, reproject and warp raster files (this is a wrapper to call [gdal\_translate](http://www.gdal.org/gdal_translate.html) or [gdalwarp](http://www.gdal.org/gdalwarp.html) basing on input parameters);
-   [`s2_mask()`](reference/s2_mask.md) to apply a cloud mask to Sentinel-2 products;
-   [`s2_calcindices()`](reference/s2_calcindices.md) to compute maps of spectral indices from Sentinel-2 Surface Reflectance multiband raster files.

Credits
-------

**fidolasen** is being developed by Luigi Ranghetti and Lorenzo Busetto ([IREA-CNR](http://www.irea.cnr.it)), and it is released under [GPL 3.0](https://www.gnu.org/licenses/gpl.html).
