# sen2r v. 1.3.2

## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.2
* [local installation] Archlinux, 64 bit, R 3.6.2
* [local installation] Windows 10, 64 bit, R 3.5.1
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.2 (devel and release)
* [win-builder] (devel and release)
* [R-hub] Windows Server 2008 R2 SP1, R-release, 32/64 bit
* [R-hub] macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs on all the tested platforms,
with the exception of win-builder oldrelease,
which returned the following warning:

Found the following significant warnings:
  Warning: namespace 'rgdal' is not available and has been replaced

This warning is quite strange, being rgdal a well-known R package working also
on R 3.5.2, for which binaries for r-oldrel are available and updated.