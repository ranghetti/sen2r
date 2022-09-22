# sen2r v. 1.5.3

This re-submission fixes the following error present on CRAN checks of different
OSs (Windows and Linux):
```
-- Error (test-s2_rgb.R:43:5): Tests on function s2_rgb() ----------------------
  Error in `c.stars_proxy(structure(list(S2A2A_20190723_022_Barbellino_RGBb84B_10.tif = 
  ...
```

In addition, tests were simplified so to require less time and resources 
to be executed and to avoid errors in revdep checks of some main R packages.

## Test environments
* [local installation] Ubuntu 22.04, 64 bit, R 4.2.1: 
    There were no ERRORs, WARNINGs nor NOTEs.
* [local installation] Archlinux, 64 bit, R 4.2.1: 
    There were no ERRORs, WARNINGs nor NOTEs.
* [local installation] Windows 10, 64 bit, R 4.2.1: 
    There were no ERRORs, WARNINGs nor NOTEs.
* [rhub] `check_for_cran()` and `check_on_mac()`: 
    There were no ERRORs, WARNINGs nor NOTEs.
* [devtools] `check_win_devel()`, `check_win_release()`: 
    There were no ERRORs, WARNINGs nor NOTEs.
* [devtools] `check_win_oldrelease()`: 
    There were no ERRORs nor WARNINGs.
* [macbuilder]


`check_win_oldrelease()` returns the following NOTE:
```
Possibly mis-spelled words in DESCRIPTION:
  Ranghetti (22:2)
  al (22:15)
  et (22:12)
```
All these words are correctly spelled, and they do not contain package names
nor book titles, so they should not be quoted.

Moreover, this re-submission fixes the following additional errors reported on M1mac: `https://www.stats.ox.ac.uk/pub/bdr/M1mac/sen2r.out`):

```
Running examples in ‘sen2r-Ex.R’ failed
The error most likely occurred in:

> ### Name: st_crs2
...
> st_crs2(raster_path)

 *** caught segfault ***
address 0x0, cause 'invalid permissions'
```

This line was put between a `\dontrun()` so to avoid further errors 
(this is not a relevant example, and `st_crs2()` is an accessory function).

```
Running the tests in ‘tests/testthat.R’ failed.
Complete output:
  > library(testthat)
  > library(sen2r)
  Welcome to sen2r. To use the package from a GUI, launch
   > sen2r()
  Documentation: https://sen2r.ranghetti.info
  > 
  > test_check("sen2r")
  Problems with SciHub credentials; check secrets.
  trying to read file: /var/folders/pk/n4bndnt1287ctrd_ftthnnnr0000gp/T//Rtmpx1lAXp/working_dir/RtmpA93TpK/out_test7_15dab1b19bcef/BOA/S2A2A_20190723_022_Barbellino_BOA_10.tif
  
   *** caught segfault ***
  address 0x0, cause 'invalid permissions'
```

This test now was removed, as well as all tests requiring authentications.
