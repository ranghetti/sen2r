#!/bin/bash

# Script used to automatically generate README.Rmd from index.Rmd
# (replacing internal links with complete links to the online documentation
# at https://ranghetti.github.io/sen2r)

# Launch the script from the main sen2r directory

sed -e "s/](\(.*\)\.md)/](https:\/\/ranghetti.github.io\/sen2r\/\1\.html)/g" index.Rmd > README.Rmd
