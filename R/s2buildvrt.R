"""
Create a vrt with all the S2 bands.

Author: Luigi Ranghetti

Year: 2017

License: GPS 3.0
"""

function s2buildvrt(infile_name,outdir=".",utmzone="") {
  
  res <- c("10m","20m","60m") # resolutions used
 
  # If a directory was passed instead of a xml, retrieve xml name automatically
  if (file.info(infile)$isdir) {
      infile_all <- list.files(infile, "\\.xml$",full.names=TRUE)
      infile <- infile_all[grep("(^S2A_.*\\_V[T0-9]+\\_[T0-9]+\\.xml$)|(^MTD\\_MSIL[12][AC]\\.xml$)",basename(infile_all),perl=TRUE)][1]
      if (length(infile_all)==0 | is.na(infile)) {
        stop("Could not find required XML file for S2 data")
      }
  } 
  # TODO check existence
  
  # retrieve UTM zone
  infile_dir = dirname(infile)
  if (utmzone=="") {
    infile_granules <- sapply(infile_granules,function(x){gsub(".*_T([0-9]{2})[A-Z]{3}_.*","\\1",x)})
    infile_utm_auto <- unique(infile_granules)
    if (length(infile_utm_auto)>1) {
      warning("More than one UTM zone was found in the product; using the first one.")
    }
    utmzone <- infile_utm_auto[1]
  }
  
  # define basename for output files
  out_prefix <- gsub(".SAFE$","",basename(infile_dir))
  
  ## Create VRT intermediate files
  infile_level <- gsub(".*L([12][AC]).*","\\1",infile)
  infile_gdalnames <- paste0("SENTINEL2_L",infile_level,":",infile,":",res,":","EPSG_326",utmzone)
  dir.create(vrt_tmpdir <- tempdir())
  vrt01_names <- file.path(vrt_tmpdir,paste0(out_prefix,"_",res,".vrt"))
  
  # create separate vrt for files
  sapply(
    paste0(
      "python2 -c \"",
      "import sys; ",
      "from osgeo import gdal; ",
      "ds = gdal.Open(sys.argv[1]); ",
      "open(sys.argv[2], 'wb').write(ds.GetMetadata('xml:VRT')[0].encode('utf-8'))\" ",
      infile_gdalnames," ",resvrt_names
    ), system, intern = Sys.info()["sysname"] == "Windows"
  )
  
  # create separate vrt for bands
  if ("10m" %in% res) {
    system(paste0("gdalbuildvrt -b 1 ",tempdir(),"/",out_prefix,"_b01.vrt ",tempdir(),"/",out_prefix,"_10m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 2 ",tempdir(),"/",out_prefix,"_b03.vrt ",tempdir(),"/",out_prefix,"_10m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 3 ",tempdir(),"/",out_prefix,"_b04.vrt ",tempdir(),"/",out_prefix,"_10m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 4 ",tempdir(),"/",out_prefix,"_b08.vrt ",tempdir(),"/",out_prefix,"_10m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
  }
  if ("20m" %in% res) {
    system(paste0("gdalbuildvrt -b 1 ",tempdir(),"/",out_prefix,"_b05.vrt ",tempdir(),"/",out_prefix,"_20m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 2 ",tempdir(),"/",out_prefix,"_b06.vrt ",tempdir(),"/",out_prefix,"_20m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 3 ",tempdir(),"/",out_prefix,"_b07.vrt ",tempdir(),"/",out_prefix,"_20m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 4 ",tempdir(),"/",out_prefix,"_b08a.vrt ",tempdir(),"/",out_prefix,"_20m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 5 ",tempdir(),"/",out_prefix,"_b11.vrt ",tempdir(),"/",out_prefix,"_20m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 6 ",tempdir(),"/",out_prefix,"_b12.vrt ",tempdir(),"/",out_prefix,"_20m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
  }
  if ("60m" %in% res) {
    system(paste0("gdalbuildvrt -b 1 ",tempdir(),"/",out_prefix,"_b01.vrt ",tempdir(),"/",out_prefix,"_60m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 2 ",tempdir(),"/",out_prefix,"_b09.vrt ",tempdir(),"/",out_prefix,"_60m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
    system(paste0("gdalbuildvrt -b 3 ",tempdir(),"/",out_prefix,"_b10.vrt ",tempdir(),"/",out_prefix,"_60m.vrt"), 
           intern = Sys.info()["sysname"] == "Windows")
  }
  
  # create final vrt
  outfile_name <- file.path(outdir,paste0(out_prefix,".vrt"))
  system(
    paste0(
      "gdalbuildvrt -separate ",
      "-resolution highest ",
      outfile_name,
      if ("60m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b01.vrt ")},
      if ("10m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b02.vrt ")},
      if ("10m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b03.vrt ")},
      if ("10m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b04.vrt ")},
      if ("20m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b05.vrt ")},
      if ("20m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b06.vrt ")},
      if ("20m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b07.vrt ")},
      if ("10m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b08.vrt ")},
      if ("20m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b08a.vrt ")},
      if ("60m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b09.vrt ")},
      if ("60m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b10.vrt ")},
      if ("20m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b11.vrt ")},
      if ("20m" %in% res) {paste0(tempdir(),"/",out_prefix,"_b12.vrt ")}
    ), intern = Sys.info()["sysname"] == "Windows"
  )
  

}