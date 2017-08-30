
# Draft for a tutorial of the package: for now it is a list of instructions
# which can be runned sequentially to reproduce the package functions.

library(sprawl)
library(data.table)
library(magrittr)
library(fidolasen)


### Set parameters ###

base_path <- "/home/lranghetti/nas-s4a/nr_working/luigi/data/s2tsp/fidolasen_tutorial"
l1c_path <- file.path(base_path, "l1c")
l2a_path <- file.path(base_path, "l2a")
vrt_path <- file.path(base_path, "vrt")
sapply(c(l1c_path,l2a_path,vrt_path),dir.create,showWarnings=FALSE)

ita_boundaries <- sprawl::get_boundaries("Italy", 1)
sel_boundaries <- ita_boundaries[ita_boundaries$NAME_1=="Valle d'Aosta",]

sel_tiles      <- c("32TLR", "32TMR")
sel_orbits     <- c(108, 65)
sel_time_window <- as.Date(c("2016-11-16","2016-12-15")) # both old and compact names
sel_prod_types <- c("BOA","TCI","SCL")

out_format     <- "VRT"
out_res        <- "10m"

### end of parameters ###


## 1) List some products in a quite extended area
#     (enough to include multiple tiles and orbits)
example_s2_list <- unlist(sapply(sel_orbits, function(x){
  s2_list(spatial_extent=sel_boundaries, time_interval=sel_time_window, orbit=x)
}))
print(example_s2_list)

## 2) Download them
s2_levels <- sapply(lapply(names(example_s2_list), s2_getMetadata, "nameinfo"), function(x){x$level})
example_s2_list_l1c <- example_s2_list[s2_levels=="1C"]
example_s2_list_l2a <- example_s2_list[s2_levels=="2A"]

if (length(example_s2_list_l1c)>0) {
  lapply(sel_tiles, function(tile) {
    s2_download(example_s2_list_l1c, out_dir=l1c_path, tile=tile)
  })
  s2_sen2cor(names(example_s2_list_l1c), l1c_dir=l1c_path, out_dir=l2a_path, n_procs=4)
}
if (length(example_s2_list_l2a)>0) {
  s2_download(example_s2_list_l2a, out_dir=l2a_path)
}

## 3) Convert in vrt
dir.create(vrt_01_path<-file.path(vrt_path,"01_translate"),showWarnings=FALSE)
for (sel_prod in list.files(l2a_path,"\\.SAFE$",full.names=TRUE)) {
  if (!file.exists(sel_prod)) {
    s2_translate(sel_prod, vrt_01_path, prod_type=sel_prod_types,
                 format="VRT", vrt_rel_paths=TRUE)
  }
}

## 4) merge by orbit
dir.create(vrt_02_path<-file.path(vrt_path,"02_merge"),showWarnings=FALSE)
vrt_01_names <- list.files(vrt_01_path, recursive=TRUE, full.names=TRUE)
s2_merge(vrt_01_names, vrt_02_path)
