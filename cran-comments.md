# sen2r v. 1.1.0

## Resubmission
This is a resubmission. Please find attached the comments made by the reviewer
(which we thanks for her effort) and our related edits.

> \dontrun{} should be only used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.
Seems mostly not necessary.
Please unwrap the examples if they are executable in < 5 sec, or create
additionally small toy examples to allow automatic testing (then replace
\dontrun with \donttest).
You could also replace \dontrun{} with \donttest, but it would be
preferable to have automatic checks for functions.

We carefully checked the use of \dontrun and \donttest within functions.
Now \donttest is used wherever automatic checks cannot be executed
(in most of the cases, where runtime GDAL binaries - which are not
installed on CRAN, Travis etc., are used).
\dontrun is instead used only if the examples are not immediately reproducible
(e.g. because example input filenames do not correspond to existing files,
or for functions which install something on disk).

Regarding package testing, we wrote test functions at tests/testthat, granting
an overall package coverage of XX% 
(most of them cannot be run automatically for the reason described above).

> Couldn't find the files necessary for gdalwarp_grid.Rd e.g.
Please add small files needed for the examples in the inst/extdata
subfolder of your package and use system.file() to get the correct
package path.

We added the directory inst/data/out, containing small
example files used by function examples, including `gdalwarp_grid.R`.
Reference documentations were improved to make use of these files.

> You have examples for unexported functions which cannot run in this way.
Please either add packagename::: to the function calls in the examples,
omit these examples or export these functions.
e.g.: gdalwarp_grid.Rd, nn.Rd

We checked all the package functions, and found this kind of problem only
in the two functions gdalwarp_grid.R and nn.R, which we fixed.

> You write information messages to the console that cannot be easily
suppressed.
Instead of print()/cat() rather use message()/warning()  or
if(verbose)cat(..) if you really have to write text to the console.
(except for print() and summary() functions)

The functions provided by the package use an internal function `print_message()`
to manage the outputs, which can write as error, warning or message.
No cat / print are explicitly used.
Different outputs could be printed by runtime executables called by R functions.

> Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). That is not allowed by CRAN policies.
Please only write/save files if the user has specified a directory in
the function themselves. Therefore please omit any default path =
getwd() in writing functions.
In your examples/vignettes/tests you can write to tempdir().

The package was writing within the package direcotry for the following reasons:
1. install Sen2Cor;
2. install aria2;
3. write the paths of the runtime dependencies in the file extdata/paths.json;
4. write the SciHub credentials in the file inst/apihub.txt;
5. write some logs in inst/extdata/logs;
6. download the file s2_tiles.rds in inst/extdata/vector/s2_tiles.rds.

These situations were managed as follows:
- first two functions were modified in order to require the output 
    path as argument;
- files 3-6 are now saved in the subfolder ".sen2r" in the user's home 
   directory; the permission to do it is now asked to the user.

> In several functions the return value is NULL, however, it would be
better to use stop() as it "stops execution of the current expression
and executes an error action.".
Please change the code (or \value{} in the documentation).

Some functions return NULL since they are not used to produce R outputs, but 
to crate/modify files, or install libraries.
Using `stop()` would generate an error, even if the functions work properly.
In these case, the entry 
`@return NULL `
was replaced with a clearer
`@return NULL (the function is called for its side effects)`.

> Please make sure that you do not change the user's options, par or
working directory. If you really have to do so, please ensure with an
*immediate* call of on.exit() that the settings are reset when the
function is exited: e.g. install_sen2cor()
...
oldwd <- getwd()           # code line i
on.exit(setwd(oldwd))      # code line i+1
...
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.

The only default value which was modified by the packages was the `setwd()` 
instruction cited by the reviewer in function `install_sen2cor`. 
This was fixed as suggested.


## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.1
* [local installation] Archlinux, 64 bit, R 3.6.1
* [local installation] Windows 10, 64 bit, R 3.5.1
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.1
* [win-builder] (devel and release)
* [R-hub] Windows Server 2008 R2 SP1, R-release, 32/64 bit
* [R-hub] macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Luigi Ranghetti <luigi@ranghetti.info>'
    New submission
