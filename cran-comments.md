# sen2r v. 1.2.1

## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.1
* [local installation] Archlinux, 64 bit, R 3.6.1
* [local installation] Windows 10, 64 bit, R 3.5.1
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.1 (devel and release)
* [win-builder] (devel and release)
* [R-hub] Windows Server 2008 R2 SP1, R-release, 32/64 bit
* [R-hub] macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results
There were no ERRORs or WARNINGs. 


There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Luigi Ranghetti <luigi@ranghetti.info>'

Found the following (possibly) invalid URLs:
  URL: http://step.esa.int/main/third-party-plugins-2/sen2cor
    From: man/install_sen2cor.Rd
          man/sen2cor.Rd
          inst/doc/installation.html
    Status: Error
    Message: libcurl error code 7:
      	Failed to connect to step.esa.int port 80: Timed out

This note could appear: http://step.esa.int seems to be affected by
network problems since some days. Nevertheless, the URL is valid.