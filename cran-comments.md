# sen2r v. 1.4.4

## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 4.0.4
* [local installation] Archlinux, 64 bit, R 4.0.4
* [local installation] Windows 10, 64 bit, R 4.0.3
* [win-builder] R unstable, 4.0.4 and 3.6.3 (devel, release and oldrelease)

## R CMD check results
There were no ERRORs nor WARNINGs.

There was 1 NOTE:
Days since last update: 1

This submission is finalised to fix an error found by CRAN checks 
on the last release submitted 1 day ago (see below).

## CRAN review
> Dear maintainer,
Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_sen2r.html>.
Please correct before 2021-05-19 to safely retain your package on CRAN.

The error were fixed
(see commit baffa72c0b76f68ec312a8069fe35da0ff9e09ef).

> You changed the system requirements but not the SystemRequirements in
1.4.3.  The message
      > test_check("sen2r")
      Searching for a valid GDAL installation...
is nonsense: there is a perfectly 'valid GDAL installation' on the
Fedora check box.  And do re-read 'Writing R Extensions' §1.6: external
software should be used conditionally, not give a check failure.
Do show more respect for the time of the CRAN team -- this reckless move
has taken up more that your fair share of CRAN time for the whole of
2021, and that is without installing the software you failed to mention
was now needed.

I am very sorry for the troubles caused by my last submission.
Runtime GDAL is not a mandatory system requirement of the sen2r package, 
but is only used for few, additional features (conditionally excluded among 
CRAN checks). It is not a new system requirement, but the error appeared in 
last version 1.4.3 due to a new instruction (checking if GDAL is installed on 
the OS) erroneously placed in the test setup file "setup-dependencies.R" 
without a conditional exclusion.
Version 1.4.4 does not check for runtime GDAL.

> There is a separate donttest error.

This was also fixed in this submitted version
(see commit 7e95bfa593fe92c3500965c17dac9f595ba2818c).
