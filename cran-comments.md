# sen2r v. 1.3.6

## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.3
* [local installation] Archlinux, 64 bit, R 4.0.0
* [local installation] Windows 10, 64 bit, R 4.0.0
* [local installation] Windows 10, 64 bit, R 3.6.3
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.2 (devel, release and oldrelease)
* [win-builder] R unstable (r78617), 4.0.0 and 3.6.3 (devel, release and oldrelease)

## R CMD check results
There were no ERRORs or WARNINGs.

A note was returned:
```
Days since last update: 3
```
this because this is a bug-fixing release:
the bug I need to fix affects the major part of Windows users, 
so this patch is urgent.
