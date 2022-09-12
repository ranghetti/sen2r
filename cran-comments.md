# sen2r v. 1.5.2

## Test environments
* [local installation] Ubuntu 22.04, 64 bit, R 4.2.1
* [local installation] Archlinux, 64 bit, R 4.2.1
* [local installation] Windows 10, 64 bit, R 4.2.1
There were no ERRORs nor WARNINGs.

`check_win_oldrelease()` returns the following NOTE:
```
Possibly mis-spelled words in DESCRIPTION:
  Ranghetti (22:2)
  al (22:15)
  et (22:12)
```
All these words are correctly spelled, and they do not contain package names
nor book titles, so they should not be quoted.

Two additional errors was reported (M1mac: `https://www.stats.ox.ac.uk/pub/bdr/M1mac/sen2r.out`):

```
Running examples in ‘sen2r-Ex.R’ failed
The error most likely occurred in:

> ### Name: st_crs2
...
> st_crs2(raster_path)

 *** caught segfault ***
address 0x0, cause 'invalid permissions'
```
I was not able to replicate the error; nevertheless, this line was put 
between a `\dontrun()` so to avoid further errors (this is not a
relevant example, and `st_crs2()` is an accessory function).

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

This test now was removed, as well as all tests requiring authentications
(commit `17bbc70adb6fcd005aff5a55d97cac1218ac387b`),
as requested by Roger Bivand (`https://github.com/ranghetti/sen2r/issues/447`).
