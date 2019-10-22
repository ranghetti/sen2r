# Version 1.2.0

Starting from this version sen2r supports ordering products from Long Term Archive (LTA)
(see news at https://inthub.copernicus.eu/userguide/LongTermArchive).
Now the user can automatically order SAFE products which are not available for direct download, and use them when made available.
Some internal functions can be exploited to manually manage orders.

Here above the related changes:

## New functions
* `safe_is_valid()` to check if an order was processed;
* `s2_order()` to order products from LTA.

## New arguments
* `sen2r()` and `s2_download()` have a new argument `order_lta` (default: TRUE) to order SAFE archives not available for direct download;
* `s2_gui()` has a new checkbox to set the previous argument.

## Other changes (not related with LTA)
* Function `build_example_param_file()` does no more compute TOA and RGB432T (this to avoid downloading 2 SAFE archives).
* Code coverage was expanded.


<br/>
## **sen2r** CRAN release
_________________________
**`sen2r`** was accepted on CRAN (2017-10-21, version 1.1.0). From now, it is possible to install the CRAN version with the following command:
```r
install.packages("sen2r")
```
The development version -- which may contain updated features -- can still be installed with the following command:
```r
remotes::install_packages_github("ranghetti/sen2r")
```


# Version 1.1.0

## Changes in default values 
* The default value for `"list_prods"` argument in `sen2r()` is NULL instead of `'BOA'`.
* Remove `'no'` value for argument `"step_atmcorr"` in `sen2r()` (use `'l2a'` without selecting any L2A products instead).
* Default values for `"smoothing"` and `"buffer"` arguments in `s2_mask()` are now `0` (like in `sen2r()`) instead than 10/20 m.
* Set `parallel = FALSE` as default in `sen2r()` (parallelisation must be explicitly required).
* Since old name products were repackaged by ESA, these products are no longer supported.
* In the case the users manually defines the path to be used as temporary directory and this directory already exists, a casual subdirectory of the user-defined folder is used (this to prevent to accidentally delete existing files).

## Major changes
* **`sen2r`** package directory is no more used to store files, but a subfolder `.sen2r` of the user Home directory is created and used. 
    In this way, reinstalling sen2r will not require anymore to reinstall runtime dependencies / to reconfigure the package. 
    The permission to write on this folder is asked to the user when the package is loaded for the first time.
* `s2_list()` and `s2_download()` now use internal methods instead of calling python function derived from s2download scripts by Hagolle (this substantially improve the speed of SAFE search), which were removed.
* **`sen2r`** is now supported also on MacOS.

## New/removed dependencies
* Remove **`sp`** and **`rgdal`** from explicit dependencies.
* Remove **`stringr`** and **`gdalUtils`** dependencies.
* Add **`stars`** dependency.

## New functions
* `str_pad2()`, equivalent to str_pad() without the needing to load "stringr".
* `raster_metadata()`, to get raster metadata, without rgdal dependency.
* `check_scihub_login()` to check if SciHub credentials are right.
* `check_scihub_connection()` to check for internet connectivity.
* `build_example_param_file()` (not exported) to build an example parameter file.

## Other changes
* Packages stability was improved by adding test functions, working with **`testthat`**.
* S2 tiles are not provided as kmz within the package, but a rds file is downloaded after installing the package (in this way, package installation is lighter, and S2 tiles are read faster).
* Function `check_gdal()` was rewritten without using **`gdalUtils`**, in order to be simpler and faster.
* Some new indices were added:
    - NDWI-NDWI2 (#184);
    - CRred-BDred-CRred2 (#168);
    - Indices from [Sentinel-hub indices](https://www.sentinel-hub.com/develop/documentation/eo_products/Sentinel2EOproducts) were checked.
* The minimum convex hull of the extent is used instead of the bbox to search SAFE images.
* Several "soft" error/warning messages were introduced.
* ENVI headers now includes some information about band names (TOA/BOA) and SCL classes.
* Coupling SAFE: in offline mode, if some L2A products exists but not the corresponding L1C, and TOA-derived products are required, only the "coupled" SAFE archives are used.
* Small base example files were added to the package for testing purposes.
* Sen2Cor version which the user wants to install can be chosen in the GUI.
* Documentation was updated and improved, in particular adding function examples.
* Unused old code was removed.
* Several bugs were fixed.


# Version 1.0.2

## Changes in default argument values
- the default running order is now `"by groups"` instead than `"step by step"`: this ensures  best computing speed and improves the management of disk usage, although it is not possible to see all the output messages in standard output (to see them, it is necessary to redirect them in a log file).
- cloud masks: `"cloud_low_proba"` is not yet available in the GUI (if is still a valid argument value for compatibility), and `"cloud_and_shadow"` now includes class "unclassified". This in order to include "unclassified" only in "conservative" mask values, and to exclude it in non-conservative ones.

## Edits in the GUI
- add commands to manage parallelisation and running order (with documentation)
- add button to export output messages in a log file
- add MathML code for some custom spectral indices

## New dependencies
- `httr`: a change in Wget available version for Windows (1.20.3 instead than 1.19.4) caused the old link to break; moreover, downloading Wget using the new link was not possible with `download.file()` due to a redirect. So, httr dependency was introduced, and download of Wget, aria2 and sen2cor is now performed with GET function.
- `dplyr`: the internal function `tiles_intersects()` now perform dissolve operations over polygons of S2 tiles using `dplyr` commands (`group_by()` and `summarise()`).

## Minor improvements
- check the list of parameters whenever it is loaded

## Various fixes
(see issues)


# Version 1.0.1

## Changes from version 1.0.0

### Major changes
- A new algorithm was implemented in `s2_calcindices()`: now internal raster routines are used by default instead than `gdal_calc.py`, allowing reducing RAM usage and computation time. 
- Better RAM usage and reduction of execution time: `sen2r_getElements()` was rewritten in order to:
    1) avoid a RAM leak present in the function, which was causing a high RAM use in the first part of the execution of `sen2r()`;
    2) considerably speed up the execution (now the metadata of 100 files is retrieved in 4 milliseconds instead than 80).

### Changes in function defaults
- `s2_calcindices()` uses internal routines; the legacy mode can be used with the argument `proc_mode = "gdal_calc"` (default value is `"raster"`).
- `list_indices()` now returns only checked indices by default (specify `all = TRUE` to return also non-checked ones).
- the default output format of `sen2r_getElements()` is now a data.table instead than a list (use `format = "list"` to reproduce the old default).

### Various fixes.


# Version 1.0.0

First stable release of package **sen2r**! See the announcement [here](https://luigi.ranghetti.info/post/sen2r-released).

## Changes from version 0.3.4

### GUI changes:
* Highlight selected tiles in the map (now red) in respect to unselected ones (grey)
* Hide TCI product (replaced by RGB images)
* Implement orbit selector and change the style of tile selector
* Make boxes collapsible
* Use shinyfiles to load extent (for coherence with other file selectors in the GUI)
* Copying `path_out` to `path_rgb` / `path_indices` is not yet necessary (if empty, `path_out` is used)
* Add loading spinner
* Other style improvements

### New functions
* `tiles_intersects()`: when an extent is loaded, now only required tiles are automatically used, instead than all the overlapping ones.

### Update dependencies
* after [V8 major update](https://www.r-bloggers.com/a-major-upgrade-of-the-v8-package/), [Installation page](https://ranghetti.github.io/sen2r/articles/installation.html) was edited to refer to `v8` instead than to `v8-3.14`

### Bug fixing:
* Fix bug in SciHub login
* Fix maps (tiles were hidden)
* Fix:missing dependency lwgeom
* Various fixes (#140, #141, #142, etc.)


# Version 0.3.4

### Major improvements
* Implement 4 different processing orders in order to fasterize processing. See "processing_order" in `sen2r()` documentation. The 4 different modes allow to privilege stability, velocity or disk space. In this version, the legacy mode ("by_step") is maintained as the default.
* Add new function `s2_rgb()` to produce RGB images, integrating it in `sen2r()` and in the GUI.
* Add a dockerised version of sen2r: https://hub.docker.com/r/ranghetti/sen2r

### Changes 
* Change base cloud masks:
    - class "Saturated and defective" is masked within all masks;
    - class "Dark area pixels" is masked in masks in which "Cloud shadows" was masked;
    - mask "cloud_shadow_cirrus" was renamed to "clear_sky";
    - a new "land" mask was added.
* Add "nomask" level to `s2_mask()`
* Names of ratio indices now uses ":" instead of "/" (#123)
* Index TCI_idx now is TCIdx
* Use a default extent name (leaving it empty is now not allowed, in order not to confuse with naming convention of merged files)

### Other improvements
* Improve SAFE filtering: removing duplicates; filtering by position (only the ones overlapping the extent); do not perform a double check of existing products in offline mode.
* Improve GUI: improve cloud mask helper (documentation within `s2_gui()` was improved by adding a modaldialog which explains how to apply buffer and smoothing to cloud masks)
* Allow sen2r working with GDAL 2.1.2
* Allow GDAL path to be specified in `check_gdal()`
* Implements a faster method for masking 
* Add max_cloud_safe argument to `sen2r()`
* Add a function `safe_isvalid()` to check for valid SAFE archive names, and use it to filter existing SAFE folders (#104)
* Update installation documentation (Linux dependencies and Docker)

### Fixes
* **Important fix**: fix indices computation (before that, indices with additive values were wrongly generated).
* Avoid errors when a SAFE directory exists but not in the right format
* Fix the fiddly error which caused several blocks when starting `s2_merge()` or `s2_calcindices()`
* Fix searching oldname products with "auto" corr_type
* Fix loading a custom mask from json parameter file
* remove duplicated EVI2 index
* #112, #115, #120, #124, and others


# Version 0.3.3

This is an improvement of version 0.3.2, with several fixes and improvements.

### Major changes
* Add internal logging (argument `log` of function `sen2r()`)
* New function `s2_dop()` to know the Dates Of Passage over orbits
* Cloud mask can now be smoothed and buffered, both using `sen2r()` and the GUI
* Add apihub parameter in sen2r() and in the GUI: It is now possible to specify a custom apihub.txt file for specific executions (i.e. for scheduled processes).

### Changes
* Minor edits in GUI
* Parallelise functions and manage errors
* Create load_binpaths() (#87: Create a function to load paths of external executables and adapt other functions to use it.
* Implement search for ingestion date
* Split names_missing into names_cloudcovered and names_missing
* Add some burn indices and update spectral indices from IDB
* Change some function names (s2_getMetadata -> safe_getMetadata; s2_shortname -> safe_shortname; fs2nc_getElements -> sen2r_getElements)

### Fixes
* Ignore baseline in computing required SAFE names
* Fix #82 (Now BOA and TOA uses GTiff intermediate files instead of VRT (gdal_calc failed using vrt as input, computing only the first band))
* Uniform some default argument values
* Various fixes


# Version 0.3.2

This is an improvement of version 0.3.1, with several fixes and improvements.

### New functions

* `check_sen2r_deps()`: a new GUI to help checking dependencies.

* Add function `s2_perc_masked()` to compute the percentage of cloud-masked surface.

### GUI changes

* Major GUI improvements in the definition of the extent: now the main map is used only to show the extent and the tiles, while the definition of the extent (drawn, specified with a bounding box or taken from a loaded polygon) is done in separate maps (this allows to avoid many errors).

* Add radiobutton for index datatype, to choose the datatype when an index is generated.

* Add maximum cloud cover threshold.

* Implement seasonal time period (#74).

* Implement the download with aria2c.

### Processing chain

* Major commit: retrieve also existing filenames.
Before this commit, if some products were partially retrieved (e.g. output BOA) and some depending on them are missing (e.g. an index) but SAFE were not required, the corresponding expected vectors were not retrieved (missing SAFE tile names nothing was generated). Now, existing names matching the parameters  are attached to each "_exp" vector.

* Manage parallelisation:
    - add partial parallelisation in s2_calcindices() (on infiles, not on indices);
    - add "parallel" argument in sen2r() to allow singlecore mode;
    - fix parallel=FALSE in function (force n_cores=1).
In general, now the whole processing chain can be optionally launched in single core.

### Addition to existing functions

* Improvement in integer / byte data types for generation of indices: 
Int16 is now the default data type; choosing an integer data type the output values are clipped on the valid format range, and the scaling factor can be chosen by the user; chosing Byte, interval -1 to 1 is coerced to 0-200 (with nodata=255).

* Use standard nodata values to compute indices.

* Return the list of created files after processing.

* Accept custom mask combinations:
now it is possible to define masks with a custom combination of SCL classes to be masked.

* Allow argument "timewindow" in `sen2r()` to be a single integer value, which is interpreted as the period between today and *n* days ago.

* Uniform temporary directories (#73).
Now the principal functions have the parameters tmpdir and rmtmp to set the temporary directory and the choice to delete /not to delete temporary files. Only two exceptions: create_indices_db() uses a fixed temporary directory (since it is a function intended not to be run by end users); sen2cor() launched from sen2r() uses a default temporary directory if tempdir is in a SAMBA mountpoint over Windows.

### Other changes

* Update to sen2cor 2.5.5, and manage S2A L2A operational products (https://scihub.copernicus.eu/news/News00305).

* Add a welcome message and a logo.

* Change package name from **SALTO** to **sen2r**, and rename the main function consequently to `sen2r()`.

* Remove s2download as dependence (now it is integrated within the package).

* Remove sprawl dependencies.

### Major fixes

* Avoid blurs and artifices in thumbnails.
stack2rgb() now works with GDAL routines; gdalwarp is used instead than gdal_translate for reducing the size of input images (to avoid averaging NA values, which produced blurs on borders of BOA thumbnails); intermediate GTiff files are used to bypass the presence of artifices in SCL PNG thumbnails and to produce JPEG multiband TOA/BOA thumbnails.

* Load python modules correctly.
Now on Windows all the modules are imported using import_from_path() in init_python(), which is called anywhere modules are needed.

* Fix unrecognised osgeo in Windows.
When running gdal_calc in Windows with multiple Python installations, the wrong one (i.e. Anaconda) is used, and "no module names osgeo" error is returned.

* Other minor fixes.


# Version 0.3.1

This is an improvement of version 0.3.1, with several fixes and secondary improvements:

Changes:
- Allows output directories to be created by fidolasen_s2 (#54)
- Convert NA arguments to NULL
- Add check that the mask polygons are not empty
- Add parameter extent_name (#58)
- Add exception for Hub unavailable
- Add support for multiple tiles and extent names
- Allow to generate thumbnails also on maps with wrong naming convention  …
- Allow to generate thumbnails also on maps with wrong naming convention  …
- Add "tiles" argument to consider only specified tiles
- Add the possibility to download products over Windows (#49)
- Add extent_name in the GUI (#58)
- Add automatic installation of Wget on Windows
- Add support for TCI thumbnail

Some fixes:
- Fix recognising gdalUtils after R restarts
- Fix error applying sen2cor on oldname SAFE
- Delete also L1C product when applying sen2cor on a copy of the SAFE
- Fix to recognise all SAFE paths
- Do not ask for overwriting dependences in non interactive mode
- Fix the check on dstnodata length
- Fix the number of polygons required to mask
- Combine multiple features in mask / extent
- Accept non-sf extents or sf with NAs within fields
- Change TCI mask values (use 0 instead of NA in order not to create problems with 8-bit format)
- Various fixes (see solved issues)


# Version 0.3.0

This version allows to perform all basic operations with Sentinel-2 data: finding and downloading the required tiles (on Linux), correcting them with sen2cor, processing them (merging adjacent tiles, clipping, reprojecting, rescaling, applying atmospheric mask) to create output multiband files (in a format managed by GDAL) and spectral indices.

The version follows the issued scheduled for [milestone 0.3.0](https://github.com/ranghetti/sen2r/milestone/1); future improvements with Sentinel-2 data are scheduled for [release 0.4.0](https://github.com/ranghetti/sen2r/milestone/3).


# Version 0.2.0

This is an intermediate version, which does not allow yet to perform a complete workflow, but it allows to manually find (`s2_list()`), download (`s2_download()`), correct (`s2_sen2cor()`), translate in a format managed by GDAL (`s2_translate()`) and merge by orbit number (`s2_merge()`) Sentinel-2 data.

An example of possible manual workflow is shown in the temporary script `WIP/tutorial.R`.
For a complete automatic workflow on Sentinel-2 data, wait [version 0.3.0](https://github.com/ranghetti/sen2r/milestone/1).
