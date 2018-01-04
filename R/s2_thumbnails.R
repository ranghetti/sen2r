
stack2rgb <- function(stack, bands=1:3, minval=0, maxval=1E4) {
  calc(stack, function(x){
    c(
      as.integer(min(max(x[[bands[1]]],minval),maxval)*255/(maxval-minval)+minval),
      as.integer(min(max(x[[bands[2]]],minval),maxval)*255/(maxval-minval)+minval),
      as.integer(min(max(x[[bands[3]]],minval),maxval)*255/(maxval-minval)+minval)
    )
  })
}


s2_thumbnails <- function(infiles, 
                          prod_type=NA, # TODO implement (for now only NOA / TOA)
                          rgb_type="SwirNirR",
                          dim=1024, 
                          outdir=NA,
                          tmpdir=NA,
                          overwrite=FALSE) {
  
  # Check that GDAL suports JPEG JFIF format
  # TODO
  
  # Set tmpdir
  if (is.na(tmpdir)) {
    tmpdir <- tempfile(pattern="dir")
    dir.create(tmpdir, recursive = FALSE, showWarnings = FALSE)
  }
  
  # Load GDAL paths
  binpaths_file <- file.path(system.file("extdata",package="fidolasen"),"paths.json")
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list("gdalinfo" = NULL)
  }
  if (is.null(binpaths$gdalinfo)) {
    check_gdal()
  }
  
  # Get files metadata
  infiles_meta <- data.table(fs2nc_getElements(infiles, format="data.frame"))
  
  out_names <- character(0) # names of created files
  for (i in seq_along(infiles)) {
    sel_infile_path <- infiles[i]
    sel_infile_meta <- c(infiles_meta[i,])

    # set outdir
    if (is.na(outdir)) {
      outdir <- file.path(dirname(sel_infile_path), "thumbnails")
      dir.create(outdir, recursive = FALSE, showWarnings = FALSE)
    }
    
    # Resize input if necessary
    sel_infile_size <- suppressWarnings(GDALinfo(sel_infile_path)[c("rows","columns")])
    if (dim < max(sel_infile_size)) {
      out_size <- round(sel_infile_size * min(dim,max(sel_infile_size)) / max(sel_infile_size))
      resized_path <- file.path(tmpdir, gsub("\\..+$",".vrt",basename(sel_infile_path)))
      system(
        paste0(
          binpaths$gdal_translate," -of VRT ",
          "-outsize ",out_size[2]," ",out_size[1]," ",
          ".r average ",
          "\"",sel_infile_path,"\" ",
          "\"",resized_path,"\""
        ), intern = Sys.info()["sysname"] == "Windows"
      )
    } else {
      resized_path <- sel_infile_path
    }
    
    # Change values to 8-bit RGB
    sel_infile <- brick(resized_path)

    # generate RGB basing on prod_type
    infile_rgb <- stack2rgb(
      sel_infile, 
      bands = switch(
        rgb_type,
        "SwirNirR" = c(11,8,4),
        "NirRG" = c(8,4,3),
        "RGB" = c(4,3,2)
      ),
      minval = 0, maxval = 1E4
    )
    
    # export
    out_path <- file.path(outdir, gsub("\\..+$",".jpg",basename(sel_infile_path)))
    suppressWarnings(
      writeGDAL(
        as(infile_rgb, "SpatialGridDataFrame"), 
        out_path,
        drivername="JPEG",
        type="Byte",
        options=c("COMPRESS=JPEG")
      )
    )
    
    out_names <- c(out_names, out_path)
    
  } # end of infiles cycle
  
  print_message(
    type="message",
    length(out_names)," output files were correctly created."
  )
  return(out_names)
  
}

# s2_thumbnails("/home/lranghetti/nas-s4a/nr_working/luigi/data/s2tsp/171030_test/out/BOA/S2B2A_20171026_022__BOA_10.tif", 
#               prod_type=NA,
#               dim=1024, 
#               outdir=NA,
#               overwrite=FALSE)

