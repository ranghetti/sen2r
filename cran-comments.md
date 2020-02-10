# sen2r v. 1.3.1

## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.2
* [local installation] Archlinux, 64 bit, R 3.6.2
* [local installation] Windows 10, 64 bit, R 3.5.1
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.2 (devel and release)
* [win-builder] (devel and release)
* [R-hub] Windows Server 2008 R2 SP1, R-release, 32/64 bit
* [R-hub] macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
Days since last update: 3

In the previous version 1.3.0 a bug was discovered:
in case the environmental variables PYTHONHOME and PYTHONPATH were not
previously set, Python-based utilities called by the package crash.
The bug interests portions of code which are commonly called by users, 
and it can affect a large part of Windows users:
this is the reason why releasing this new version 1.3.1, 
which corrects this bug, is so urgent.
