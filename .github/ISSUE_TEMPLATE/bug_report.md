---
name: Bug report
about: Create a report to help us improve
title: ''
labels: bug
assignees: ''

---

<!--
Use this template to report a bug. Please use this method instead than sending private email to the authors.

Before opening a new issue please check if the problem was already been mentioned (if so but the found issue is closed, open a new issue citing the old task instead than reopening it).

Ensure that your {sen2r} version is update with the newest GitHub master branch:
install.packages("remotes")
remotes::install_github("ranghetti/sen2r")

Please take particular care with code reproducibility (follow indications provided in the template).
-->

**Bug description**
<!-- Add here a clear and concise description of what the bug is. -->

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
