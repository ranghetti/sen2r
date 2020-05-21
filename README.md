
<!-- IMPORTANT: do NOT edit README.Rmd! Edit index.Rmd instead, -->

<!-- and generate README.Rmd using utils/code/create_README.sh  -->

[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/sen2r)](https://cran.r-project.org/package=sen2r)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1240384.svg)](https://doi.org/10.5281/zenodo.1240384)
[![Travis-CI Build
Status](https://travis-ci.org/ranghetti/sen2r.svg?branch=master)](https://travis-ci.org/ranghetti/sen2r)
[![Coverage
Status](http://img.shields.io/codecov/c/github/ranghetti/sen2r/master.svg)](http://codecov.io/github/ranghetti/sen2r?branch=master)
[![Docker Automated
build](https://img.shields.io/docker/automated/ranghetti/sen2r.svg)](https://hub.docker.com/r/ranghetti/sen2r)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v1.4%20adopted-ff69b4.svg)](http://sen2r.ranghetti.info/.github/CODE-OF-CONDUCT.html)

<img src="man/figures/sen2r_logo_200px.png" width="200" height="113" align="right" />

# sen2r: Find, Download and Process Sentinel-2 Data

<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
is an R library which helps to download and preprocess Sentinel-2
optical images. The purpose of the functions contained in the library is
to provide the instruments required to easily perform (and eventually
automate) all the steps necessary to build a complete Sentinel-2
processing chain, without the need of any manual intervention nor the
needing to manually integrate any external tool.

In particular,
<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
allows to:

  - retrieve the list of available products on a selected area (which
    can be provided by specifying a bounding box, by loading a vector
    file or by drawing it on a map) in a given time window;
  - download the required SAFE Level-1C products, or retrieve the
    required SAFE Level-2A products by downloading them (if available)
    or downloading the corresponding Level-1C and correcting them with
    **Sen2Cor**;
  - obtain the required products (Top of Atmosphere radiances, Bottom of
    Atmosphere reflectances, Surface Classification Maps, True Colour
    Images) clipped on the specified area (adjacent tiles belonging to
    the same frame are merged);
  - mask cloudy pixels (using the Surface Classification Map as masking
    layer);
  - computing spectral indices and RGB images.

Setting the execution of this processing chain is particularly easy
using the
<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
GUI, which allows to set the parameters, to directly launch the main
function or to save them in a JSON file which can be used to launch the
processing at a later stage.

The possibility to launch the processing with a set of parameters saved
in a JSON file (or directly passed as function arguments) makes easy to
build scripts to automatically update an archive of Sentinel-2 products.
Specific processing operations (i.e. applying **Sen2Cor** on Level-1c
SAFE products, merging adjacent tiles, computing spectral indices from
existing products) can also be performed using intermediate functions
(see
[usage](#usage)).

<a href="https://www.sciencedirect.com/science/article/pii/S0098300419304893" target="_blank"><img style="margin-right:20px;" src="data:image/gif;base64,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" width="120" height="160" align="left" /></a>

<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
was published on the ISI journal *Computers & Geosciences*; the
manuscript is available
here:

<a href="https://www.sciencedirect.com/science/article/pii/S0098300419304893" target="_blank">
L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
<strong>“sen2r: An R toolbox for automatically downloading and
preprocessing Sentinel-2 satellite data”</strong>. <em>Computers &
Geosciences</em>, 139, 104473. DOI: 10.1016/j.cageo.2020.104473, URL:
http://sen2r.ranghetti.info </a>.

Users which processed Sentinel-2 data and used them for a scientific
work are strongly encouraged to cite this publication within their work
(see [Credits](#credits)).

## Installation

The package can be installed from CRAN:

``` r
install.packages("sen2r", dependencies = TRUE)
```

Note that argument `dependencies = TRUE` is needed to be able to run all
the package’s features.

For detailed instructions about installing the package (including
dependencies), see the
[Installation](http://sen2r.ranghetti.info/articles/installation.html)
page.

A dockerised version of
<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
is available [here](https://hub.docker.com/r/ranghetti/sen2r). For
detailed instructions about using it, see the page [“Run in a Docker
container”](http://sen2r.ranghetti.info/articles/docker.html).

## Usage

The simplest way to use
<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
is to execute it in interactive mode (see [this
vignette](http://sen2r.ranghetti.info/articles/sen2r_gui.html)):

``` r
library(sen2r)
sen2r()
```

<p style="text-align:center;">

<a href="https://raw.githubusercontent.com/ranghetti/sen2r/devel/man/figures/sen2r_gui_sheet1.png" target="_blank">
<img src="man/figures/sen2r_gui_sheet1_small.png"> </a>
<a href="https://raw.githubusercontent.com/ranghetti/sen2r/devel/man/figures/sen2r_gui_sheet2.png" target="_blank">
<img src="man/figures/sen2r_gui_sheet2_small.png"> </a> <br/>
<a href="https://raw.githubusercontent.com/ranghetti/sen2r/devel/man/figures/sen2r_gui_sheet3.png" target="_blank">
<img src="man/figures/sen2r_gui_sheet3_small.png"> </a>
<a href="https://raw.githubusercontent.com/ranghetti/sen2r/devel/man/figures/sen2r_gui_sheet4.png" target="_blank">
<img src="man/figures/sen2r_gui_sheet4_small.png"> </a>
<a href="https://raw.githubusercontent.com/ranghetti/sen2r/devel/man/figures/sen2r_gui_sheet5.png" target="_blank">
<img src="man/figures/sen2r_gui_sheet5_small.png"> </a>

</p>

Alternatively,
[`sen2r()`](http://sen2r.ranghetti.info/reference/sen2r.html) can be
launched with a list of parameters (created with
[`s2_gui()`](http://sen2r.ranghetti.info/reference/s2_gui.html)) or
passing manually the parameters as arguments of the function (see [this
vignette](http://sen2r.ranghetti.info/articles/sen2r_cmd.html) for
further details).

Other specific functions can be used to run single steps separately.
This is a list of the principal processing functions (the complete list
can be found in the Reference page):

  - [`s2_list()`](http://sen2r.ranghetti.info/reference/s2_list.html) to
    retrieve the list of available Sentinel-2 products based on input
    parameters;
  - [`s2_download()`](http://sen2r.ranghetti.info/reference/s2_download.html)
    to download Sentinel-2 products;
  - [`s2_order()`](http://sen2r.ranghetti.info/reference/s2_order.html)
    to order products from the Long Term Archive;
  - [`sen2cor()`](reference/sen2cor.html) to correct level-1C products
    using
    [Sen2Cor](http://step.esa.int/main/third-party-plugins-2/sen2cor);
  - [`s2_mask()`](http://sen2r.ranghetti.info/reference/s2_mask.html) to
    apply a cloud mask to Sentinel-2 products;
  - [`s2_rgb()`](http://sen2r.ranghetti.info/reference/s2_rgb.html) to
    generate RGB images from Sentinel-2 Surface Reflectance multiband
    raster
    files;
  - [`s2_calcindices()`](http://sen2r.ranghetti.info/reference/s2_calcindices.html)
    to compute maps of spectral indices from Sentinel-2 Surface
    Reflectance multiband raster files.

Output products follow a specific naming convention (see
[here](http://sen2r.ranghetti.info/articles/outstructure.html) for
details).

## Credits

<a href="http://www.irea.cnr.it" target="_blank">
<img src="man/figures/irea_logo_200px.png" height="100" align="left" style="padding-right: 100px;"/></a>
<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
is being developed by Luigi Ranghetti and Lorenzo Busetto
([IREA-CNR](http://www.irea.cnr.it)), and it is released under the [GNU
General Public License
version 3](https://www.gnu.org/licenses/gpl-3.0.html) (GPL‑3).

The
[<span style="color:#5793dd;vertical-align:top;font-size:90%;font-weight:normal;">sen</span><span style="color:#6a7077;vertical-align:baseline;font-size:115%;font-weight:bolder;">2</span><span style="color:#2f66d5;vertical-align:baseline;font-size:90%;font-weight:bold;">r</span>
logo](https://raw.githubusercontent.com/ranghetti/sen2r/devel/man/figures/sen2r_logo_200px.png),
partially derived from the [R logo](https://www.r-project.org/logo), is
released under the [Creative Commons Attribution-ShareAlike 4.0
International license](https://creativecommons.org/licenses/by-sa/4.0)
(CC-BY-SA 4.0).

To cite this library, please use the following entry:

L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020). **“sen2r: An R
toolbox for automatically downloading and preprocessing Sentinel-2
satellite data”**. *Computers & Geosciences*, 139, 104473. DOI:
<a href="https://dx.doi.org/10.1016/j.cageo.2020.104473" target="_blank">10.1016/j.cageo.2020.104473</a>,
URL:
<a href="http://sen2r.ranghetti.info" target="_blank">http://sen2r.ranghetti.info</a>.

``` bibtex
@Article{sen2r_cageo,
  title   = {sen2r: An R toolbox for automatically downloading and preprocessing Sentinel-2 satellite data},
  author  = {Luigi Ranghetti and Mirco Boschetti and Francesco Nutini and Lorenzo Busetto},
  journal = {Computers & Geosciences},
  year    = {2020},
  volume  = {139},
  pages   = {104473},
  doi     = {10.1016/j.cageo.2020.104473},
  url     = {http://sen2r.ranghetti.info/},
}
```

## Contributing

This project is released with a [Contributor Code of
Conduct](http://sen2r.ranghetti.info/.github/CODE-OF-CONDUCT.html). By
participating in this project you agree to abide by its terms.

Users are encouraged to use GitHub issues in case of errors with the
package. Before opening a new issue, please read
<a href="https://github.com/ranghetti/sen2r/issues/186" target="_blank">these
notes</a>.
