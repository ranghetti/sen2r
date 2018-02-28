
# Draft for a tutorial of the package: for now it is a list of instructions
# which can be runned sequentially to reproduce the package functions.

# devtools::install_github("lbusett/sprawl", ref="b7f1205cb0925d7fdcedb7ba1ac7a02825b415d2", force=TRUE)

library(sprawl)
library(data.table)
library(magrittr)
library(salto)


### Set parameters ###

base_path <- "/home/lranghetti/nas-s4a/nr_working/luigi/data/s2tsp/fidolasen_tutorial"
l1c_path <- file.path(base_path, "l1c")
l2a_path <- file.path(base_path, "l2a")
vrt_path <- file.path(base_path, "vrt")
out_path <- file.path(base_path, "out")
sapply(c(l1c_path,l2a_path,vrt_path),dir.create,showWarnings=FALSE)

ita_boundaries <- sprawl::get_boundaries("Italy", 3)
sel_crop <- ita_boundaries[grep("Valtournenche|Ayas|Gressoney-La-Trinit[èé]", ita_boundaries$NAME_3),]

sel_tiles      <- c("32TLR", "32TMR")
sel_orbits     <- c(108, 65)
sel_time_window <- as.Date(c("2016-11-16","2016-12-15")) # both old and compact names
sel_prod_types <- c("BOA","TCI","SCL")
sel_indices    <- c("NDVI","SAVI","NDRE","EVI")

out_format     <- "GTiff"
sel_res        <- "10m"

out_res        <- 15
out_proj       <- sp::CRS("+init=epsg:3035")
out_bbox       <- sel_crop %>% get_extent() %>% reproj_extent(out_proj) %>% as("matrix")


### end of parameters ###


## 1) List some products in a quite extended area
#     (enough to include multiple tiles and orbits)
example_s2_list <- unlist(lapply(sel_orbits, function(x){
  s2_list(spatial_extent=sel_crop, time_interval=sel_time_window, orbit=x)
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
  s2_sen2cor(list.files(l1c_path,"\\.SAFE$"), l1c_dir=l1c_path, out_dir=l2a_path, n_procs=1)
}
if (length(example_s2_list_l2a)>0) {
  s2_download(example_s2_list_l2a, out_dir=l2a_path)
}

## 3) Convert in vrt
dir.create(vrt_01_path<-file.path(vrt_path,"01_translate"),showWarnings=FALSE)
for (sel_prod in list.files(l1c_path,"\\.SAFE$",full.names=TRUE)) {
  if (!file.exists(file.path(vrt_01_path,basename(s2_shortname(sel_prod, ext="vrt"))))) { # FIXME
    s2_translate(sel_prod, vrt_01_path, prod_type="TOA",
                 format="VRT", vrt_rel_paths=TRUE, subdirs=TRUE)
  }
}
for (sel_prod in list.files(l2a_path,"\\.SAFE$",full.names=TRUE)) {
  if (!file.exists(file.path(vrt_01_path,basename(s2_shortname(sel_prod, ext="vrt"))))) { # FIXME
    s2_translate(sel_prod, vrt_01_path, prod_type=sel_prod_types,
                 format="VRT", vrt_rel_paths=TRUE)
  }
}

## 4) merge by orbit
dir.create(vrt_02_path<-file.path(vrt_path,"02_merge"),showWarnings=FALSE)
vrt_01_names <- list.files(vrt_01_path, recursive=TRUE, full.names=TRUE)
s2_merge(vrt_01_names, vrt_02_path)

## 5) clip, rescale, reproject
dir.create(vrt_03_path<-file.path(vrt_path,"03_warp"),showWarnings=FALSE)
vrt_02_names <- list.files(vrt_02_path, recursive=TRUE, full.names=TRUE)
vrt_03_names <- file.path(vrt_03_path,
                          basename(dirname(vrt_02_names)),
                          basename(vrt_02_names))
sapply(unique(basename(dirname(vrt_03_names))), function(x) {
  dir.create(file.path(vrt_03_path,x),showWarnings=FALSE)
})
salto::gdal_warp(vrt_02_names, vrt_03_names,
                     t_srs = out_proj@projargs,
                     mask = sel_crop,
                     tr = c(out_res,out_res),
                     of = "VRT",
                     compress="DEFLATE")
# lapply(sel_outfiles, gdal_abs2rel) # FIXME move within single functions

## 5) Apply mask
sapply(subdirs <- unique(basename(dirname(vrt_03_names))), function(x) {
  dir.create(file.path(out_path,x),showWarnings=FALSE)
})

s2_mask(vrt_03_names[grep("BOA|TCI|TOA",vrt_03_names)],
        vrt_03_names[grep("_SCL_",vrt_03_names)],
        mask_type="cloud_medium_proba",
        outdir=out_path,
        format=out_format,
        subdirs=TRUE)


## 5) Compute spectral indices
boa_names <- list.files(out_path, "BOA", recursive=TRUE, full.names=TRUE)
s2_calcindices(boa_names, sel_indices, out_path, subdirs=TRUE, format="GTiff", compress="DEFLATE", dataType="Int32")



### Tests on gdal_warp ###
srcfiles <- list.files(out_path, "NDVI", recursive=TRUE, full.names=TRUE)
crop_poly <- sel_crop[sel_crop$NAME_3=="Ayas",]
srcfiles <- srcfiles[grep("20161211.+tif$",srcfiles)]
# then follows the examples in gdal_warp documentation
