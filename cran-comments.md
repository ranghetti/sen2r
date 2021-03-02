# sen2r v. 1.4.1

## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 4.0.4
* [local installation] Archlinux, 64 bit, R 4.0.4
* [local installation] Windows 10, 64 bit, R 4.0.3
* [win-builder] R unstable, 4.0.4 and 3.6.3 (devel, release and oldrelease)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.


## CRAN review
> Dear maintainer,
Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_sen2r.html>.
Please correct before 2021-03-11 to safely retain your package on CRAN.

All the errors were fixed (they were due to #390)
(see commit 82754724fd3259b12ea1aee57a5242cfcec8438d).

> 'Writing R Extensions' asked you not to use progress bars in
non-interactive sessions and doing so makes your erroneous output
necessarily hard to read.

All progress bars were put within `if (interactive())` checks 
(see commit c4f36378859d584d9c617ccdd3938d4086a05816).
