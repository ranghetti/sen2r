# sen2r v. 1.1.0

## Resubmission
This is a resubmission. Please find attached the comments made by the reviewer
(which we thanks for her effort) and our related edits.

> Thanks, please check whether sen2cor (and maybe other software) has to
be declared in the SystemRequirements field of the DESCRIPTION file.

Sen2Cor is not a system dependency, in the sense that there is not an installer
which have to be run with administration rules (Windows) or a package which can
be installed with a package manager or a source code to be compiled (Unix),
but there is an archive which can be manually extracted with user permissions
and run by the user who extracted it.
In other words, Sen2Cor cannot be found with a sys.which call.
Moreover, it is not a mandatory requirement, while the package can be used
without it if the user does not require to perform an atmospheric correction 
of Sentinel-2 images.
So, in our opinion, it should not be declared in the SystemRequirements 
field of the DESCRIPTION file.

> Please do not install software in your tests.

We commented the test code for the function `install_sen2cor()`
(commit f21281230d093911d47ce43b3f64a64ce23bc146).


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
    New submission
