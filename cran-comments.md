# sen2r v. 1.1.0

## Resubmission
This is a resubmission. In this version:

* the title was edited as requested by the reviewer
    (from "An R Toolbox to Find, Download and Process Sentinel-2 Data"
    to "Find, Download and Process Sentinel-2 Data"),
    not only in DESCRIPTION but in the whole documentation;
    
* invalid file URI `docker.md` from inst/doc/installation.html was fixed;

* the description was not edited, sice we have not yet any reference to methods 
    to add in (we submitted a paper regarding them, but currently it is under review).


## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.1
* [local installation] Archlinux, 64 bit, R 3.6.1
* [local installation] Windows 10, 64 bit, R 3.5.1
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.1
* [win-builder] (devel and release)
* [R-hub] Windows Server 2008 R2 SP1, R-release, 32/64 bit
* [R-hub] macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Luigi Ranghetti <ranghetti.l@irea.cnr.it>'
    New submission
