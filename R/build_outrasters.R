
# INTERNAL
# the preprocessing functions (s2_translate, s2_merge) were build in order
# to deal with both raster and virtual formats;
# however, using functions in sequence it is expeceted that they will manage virtual raster.
# this function build the final output rasters from vrts:
# it makes simple preprocessing operations (clip) and produces final raster with
# all the parameters needed by the chosen format (like compression).
# if some operations will result in more than a few lines, split in a separate function.
