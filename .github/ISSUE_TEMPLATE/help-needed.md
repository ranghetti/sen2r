---
name: Help needed
about: Use this in case of problems using {sen2r}
title: ''
labels: assistance
assignees: ''

---

<!--
Use this template if you need assistance running {sen2r} on your code (e.g., in case of errors which are not a bug / you are not sure if they are a bug). Please use this method instead than sending private email to the authors.

Before opening a new issue please check if the problem was already been mentioned (if so but the found issue is closed, open a new issue citing the old task instead than reopening it).

Ensure that your {sen2r} version is update with the last CRAN version:
install.packages("sen2r")

Ensure that the installation / configuration was performed correctly (follow https://sen2r.ranghetti.info/articles/installation.html ).

Please take particular care with code reproducibility (follow indications provided in the template).

NOTES ABOUT EXTERNAL DEPENDENCIES
1. Please note that the use of an external GDAL environment (required only to smooth / buffer a cloud mask, and optionally to compute spectral indices, RGB images and thumbnails) is no more supported: maintainers are not responsible in case of GDAL-related issues.
2. Sen2Cor can be launched from {sen2r}, but it is an independent software: in case of problems with it, ensure that this is related to {sen2r} and not to Sen2Cor before opening an issue.

IMPORTANT NOTES
1. Please remember that {sen2r} is not a commercial tool, so developers are not obliged to provide assistance: please be polite, be patient if developers will not answer you instantly and respect the Code of Conduct (https://sen2r.ranghetti.info/CODE-OF-CONDUCT.html)
2. Your are required to answer us when we required details (generally outputs of R commands)  and to provide us a feedback after opening an issue, even after solving your problem or if you are not yet interested in solving it. In the case of missing feedback, we reserve the right to ignore your future requests.
3. Tasks can be closed after 10 days of inactivity (you can reopen it if you need further help).
-->

**Issue description**
<!-- Add here a clear and concise description of what the problem is about. -->

**Reproducible example**
<!-- Please provide here a reproducible example in the chunk below. -->

```r
## PLEASE DELETE AND WRITE YOUR OWN
library(sen2r)
example_extent <- "C:/path/of/the/file/example.shp"
# file example.shp _must_ be attached if not available online
output <- sen2r("/path/of/the/parameter_file.json")
# file parameter_file.json _must_ be attached of copied as text
```

**Expected and actual behavior**
<!-- Provide here the full output of the provided example and describe what is going wrong- -->

```
## PLEASE DELETE AND WRITE YOUR OWN OUTPUT
[2020-12-21 16:17:01] #### Starting sen2r execution. ####
[2020-12-21 16:17:02] Searching for available SAFE products on SciHub...
[2020-12-21 16:17:10] Computing output names...
...
```

**System information**
<!-- Provide here the output of the following R commands:
sessionInfo()
packageVersion("sen2r")
sen2r::load_binpaths()
 -->

```
# PASTE HERE YOUR OUTPUT
```

**Additional context**
<!-- Add here any other context about the problem here (for example, the content of the output folder in case the error appears during a subsequent code execution. -->
