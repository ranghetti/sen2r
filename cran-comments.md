# sen2r v. 1.3.0

## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.1
* [local installation] Archlinux, 64 bit, R 3.6.1
* [local installation] Windows 10, 64 bit, R 3.5.1
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.1 (devel and release)
* [win-builder] (devel and release)
* [R-hub] Windows Server 2008 R2 SP1, R-release, 32/64 bit
* [R-hub] macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results

There was 1 NOTE:
```
* checking dependencies in R code ... NOTE
Missing or unexported object: 'rgdal::showSRID'
```

This note is due to the fact that 'showSRID' is a function implemented
in `rgdal >= 1.5-2` (available at http://R-Forge.R-project.org/).
This function is necessary to correctly handle WKT2 representations of CRS
starting from PROJ >= 3 (http://rgdal.r-forge.r-project.org/articles/PROJ6_GDAL3.html),
but can not be included in NAMESPACE since CRAN version of `rgdal` is 1.4-8.

Nevertheless:

1. **rgdal** is a dependence of sen2r;
2. `showSRID()` can not be called by **sen2r** unless the r-forge version of 
    package **rgdal** was manually installed: in fact, the function is called
    only in the following `if` cycle:
```
if (all(
  package_version(sf_extSoftVersion()["proj.4"]) >= 6,
  packageVersion("rgdal") >= 1.5
))
```
        
