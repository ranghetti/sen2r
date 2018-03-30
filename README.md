
-   [An R toolbox to find, download and preprocess Sentinel-2 data](#an-r-toolbox-to-find-download-and-preprocess-sentinel-2-data)
    -   [Warning](#warning)
    -   [Installation](#installation)
    -   [Usage](#usage)
    -   [Credits](#credits)

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/ranghetti/sen2r.svg?branch=master)](https://travis-ci.org/ranghetti/sen2r) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sen2r)](https://cran.r-project.org/package=sen2r) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

<img src="man/figures/sen2r_logo_200px.png" width="200" height="113" align="right" />

An R toolbox to find, download and preprocess Sentinel-2 data
=============================================================

Warning
-------

This package is under construction (current version is [pre-release 0.3.1](https://github.com/ranghetti/sen2r/releases/tag/v0.3.1)): currently only the main processing chain for Sentinel-2 data was implemented. Please refer to the [crontab of release 0.4.0](https://github.com/ranghetti/sen2r/milestone/3) to know which implementations will be performed regarding Sentinel-2 data.

Installation
------------

You can install <span style="color:#5793dd;vertical-align:text-bottom;font-size:90%;font-weight:500;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:850;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:700;">r</span> from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("ranghetti/sen2r")
```

This will install the R package, containing all the functions necessary to preprocess data.

To download Sentinel-2 images the package makes use of a set of Python functions ([s2download](https://github.com/ranghetti/s2download)). These scripts are imported the first time hey are used (alternatively, launch the function [`s2_download()`](reference/install_s2download.md) included in the package).

Atmospheric correction is performed using [sen2cor](http://step.esa.int/main/third-party-plugins-2/sen2cor): the package will automatically download and install it at first use, or by running function [`install_sen2cor()`](reference/install_sen2cor.md). Please notice that the use of sen2cor algorythm was not yet possible under MAC.

Preprocessing functions make use of [GDAL](http://www.gdal.org), which must support JPEG2000 format. On Windows, it is strongly recommended to install it using the [OSGeo4W installer](http://download.osgeo.org/osgeo4w/osgeo4w-setup-x86_64.exe) in advanced mode, and checking the installation of `openjpeg` library.

Usage
-----

The simpler way to use <span style="color:#5793dd;vertical-align:text-bottom;font-size:90%;font-weight:500;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:850;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:700;">r</span> is to execute the function `sen2r()` without any argument: this opens the GUI to select the processing parameters, and then launches the main function.

Alternatively, [`sen2r()`](reference/sen2r.md) can be launched with a list of parameters (created with [`s2_gui()`](reference/s2_gui.md)) or passing manually the parameters as arguments of the function (see the documentation of the function for further details).

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

<span style="color:#5793dd;vertical-align:text-bottom;font-size:90%;font-weight:500;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:850;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:700;">r</span> is being developed by Luigi Ranghetti and Lorenzo Busetto ([IREA-CNR](http://www.irea.cnr.it)), and it is released under the [GNU General Public License version 3](https://www.gnu.org/licenses/gpl-3.0.html) (GPL‑3).

The [<span style="color:#5793dd;vertical-align:text-bottom;font-size:90%;font-weight:500;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:850;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:700;">r</span> logo](man/figures/sen2r_logo_200px.png), partially derived from the [R logo](https://www.r-project.org/logo), is released under the [Creative Commons Attribution-ShareAlike 4.0 International license](https://creativecommons.org/licenses/by-sa/4.0) (CC-BY-SA 4.0).
