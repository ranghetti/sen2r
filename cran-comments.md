# sen2r v. 1.5.5


## Test environments
* [local installation] Windows 11, 64 bit, R 4.3.0: 
  There were no ERRORs, WARNINGs nor NOTEs.
* [local installation] Ubuntu 22.04.2, 64 bit, R 4.3.0: 
  There were no ERRORs, WARNINGs nor NOTEs.
* [macbuilder] macOS 13.3.1 (Mac mini Apple M1), 64 bit, R 4.3.0
  (`https://mac.r-project.org/macbuilder/results/1686346670-b2477c89982a7f19/`):
  There were no ERRORs, WARNINGs nor NOTEs.
* [devtools] `check_win_devel()`
  (``): 
  There were no ERRORs nor WARNINGs (1 NOTE, see below).
* [devtools] `check_win_release()`
  (``): 
  There were no ERRORs nor WARNINGs (1 NOTE, see below).
* [devtools] `check_win_oldrelease()`
  (``): 
  There were no ERRORs nor WARNINGs (1 NOTE, see below).

`check_win_oldrelease()`, `check_win_release()` and `check_wind_devel()` return 
the following NOTE:
```
* checking CRAN incoming feasibility ... [13s] NOTE
Maintainer: 'Luigi Ranghetti <rpackages.ranghetti@gmail.com>'

New maintainer:
 Luigi Ranghetti <rpackages.ranghetti@gmail.com>
Old maintainer(s):
 Luigi Ranghetti <sen2r@ranghetti.info>
```
The email was changed in order to use an account which does not forward to 
Google, so to respect CRAN requirements.

`check_win_oldrelease()` returns the following NOTE:
```
Possibly mis-spelled words in DESCRIPTION:
 Ranghetti (22:2)
 al (22:15)
 et (22:12)
```
All these words are correctly spelled, and they do not contain package names
nor book titles, so they should not be quoted.