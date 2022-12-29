# sen2r v. 1.5.4


## Test environments
* [local installation] Windows 11, 64 bit, R 4.2.1: 
  There were no ERRORs, WARNINGs nor NOTEs.
* [local rocker/geospatial] Ubuntu 22.04.1, 64 bit, R 4.2.2: 
  There were no ERRORs, WARNINGs nor NOTEs.
* [macbuilder] macOS 11.5.2 (Mac mini Apple M1), 64 bit, R 4.2.1
  (`https://mac.r-project.org/macbuilder/results/1672324384-381ee107a5dfe5fd/`):
  There were no ERRORs, WARNINGs nor NOTEs.
* [devtools] `check_win_devel()`
  (`https://win-builder.r-project.org/X178ypsB7Ij0/`): 
  There were no ERRORs nor WARNINGs (1 NOTE, see below).
* [devtools] `check_win_release()`
  (`https://win-builder.r-project.org/s81ObAlWW438/`): 
  There were no ERRORs nor WARNINGs (1 NOTE, see below).
* [devtools] `check_win_oldrelease()`
  (`https://win-builder.r-project.org/ZAH1HwJq84h8/`): 
  There were no ERRORs nor WARNINGs (1 NOTE, see below).
* [rhub] `check_on_macos()`
  (`https://builder.r-hub.io/status/sen2r_1.5.4.tar.gz-967caad4a7534a2299bc3ace102062f8`): 
  There were no ERRORs, WARNINGs nor NOTEs.

`check_win_release()` and `check_wind_devel()` return the following NOTE:
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