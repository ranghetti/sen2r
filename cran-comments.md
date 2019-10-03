# sen2r v. 1.1.0

## Resubmission
This is a resubmission. Please find attached the comments made by the reviewer
(which we thanks for her effort) and our related edits.

> Please always make sure to reset to user's options, wd or par after you
changed it in examples and vignettes.
e.g. in gdalwarp_grid.Rd, and more...:
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)

We checked the use of `par()`, restoring the previous settings whenever it
was used (commit a778ef2c0934ff6df27c666a94346098cfa221e5).
Working directory and user options were never changed.

> Please add all system requirements to the DESCRIPTION file.
"Dependencies external to the R system should be listed in the
‘SystemRequirements’ field, [...]".

System requirements were added to the DESCRIPTION file
(commit e2f1f82c52af13238b6ab3088d54b1c77704a4a9).

> "\dontrun is instead used only if the examples are not immediately reproducible
(e.g. because example input filenames do not correspond to existing files,
or for functions which install something on disk)."
Also necassary in safe_getMetadata.Rd?
In this example also use system.file() to get a path.

Function `safe_getMetadata()` requires the SAFE archive to exist,
unless `info` argument is set to `"nameinfo"`.
We cannot add a sample SAFE archive to the package (so referring to it using
`system.file()`), due to the huge file size of SAFE archives.
So, `\dontrun{}` is necessary for the examples in which `info != "nameinfo"`.
Nevertheless, we edited the examples
(commit 1d2af51e9f00c7ac98778cf9e2834af0e72c662d)
in order to provide the instructions for a sample SAFE download
(included in `\dontrun{}` for the reason above),
so that examples can refer to it instead than to a fake SAFE path.

>>> Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). That is not allowed by CRAN policies.
[...]
>> 
>> The package was writing within the package direcotry for the following reasons:
>> 1. install Sen2Cor;
>> 2. install aria2;
>> [...]
>> These situations were managed as follows:
>> - first two functions were modified in order to require the output 
    path as argument;
>> [...]
> 
> That is great. However, now the examples lack directories. Please add
tempdir() and write a comment for users to change the directory.

This was done in commit 368a9fa68e9c4d2aef297ca3aff2e5cd8f117f19.

>> Some functions return NULL since they are not used to produce R outputs, but
to crate/modify files, or install libraries.
Using `stop()` would generate an error, even if the functions work properly.
In these case, the entry 
`@return NULL `
was replaced with a clearer
`@return NULL (the function is called for its side effects)`.
> 
> I'm not sure this happend also in safe_getMetadata()?

Yes. In this function, the argument `abort` determines if the function would
return a warning (if `abort = FALSE`) or a message (if `abort = TRUE`) 
before exiting. For this reason, the syntax
```
print_message(
  type=message_type,
  "Exit message"
)
return(invisible(NULL))
```
is used instead than
```
stop("Exit message")
```
The two are equivalent in case `abort = TRUE`.


## Test environments
* [local installation] Ubuntu 18.04, 64 bit, R 3.6.1
* [local installation] Archlinux, 64 bit, R 3.6.1
* [local installation] Windows 10, 64 bit, R 3.5.1
* [travis-ci] Ubuntu 16.04.6 LTS, 64 bit, R 3.6.1 (devel and release)
* [win-builder] (devel and release)
* [R-hub] Windows Server 2008 R2 SP1, R-release, 32/64 bit
* [R-hub] macOS 10.11 El Capitan, R-release (experimental)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Luigi Ranghetti <luigi@ranghetti.info>'
    New submission
