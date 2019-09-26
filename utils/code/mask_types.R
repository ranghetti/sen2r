
# Internal code used to generate the images for mask comparison

library(sen2r)
library(ggimage)

# Define directories and parameters
safe_dir <- file.path(dirname(attr(load_binpaths(), "path")), "safe")
dir.create(safe_dir, showWarnings = FALSE)
out_dir <- tempfile(pattern = "out_masktypes_")
dir.create(out_dir)

sel_extent <- st_as_sfc(st_bbox(
  c("xmin"=583E3, "ymin"=5094E3, "xmax"=590E3, "ymax"=5098E3),
  crs = st_crs(32632)
))

# Run sen2r to generate outputs
for (s in c(0,30,100)) {
  for (b in c(0,30,100)) {
    sen2r(
      gui = FALSE,
      online = TRUE,
      s2tiles_selected = "32TNR",
      s2_levels = "l2a",
      step_atmcorr = "l2a",
      extent = sel_extent,
      extent_name = paste0("s",str_pad2(s,3,"left","0"),"b",str_pad2(b,3,"left","0")),
      extent_as_mask = FALSE,
      timewindow = as.Date("2018-08-05"),
      list_prods = c("TCI","SCL"),
      mask_type = "scl_0_1_2_3_7_8_9_11", mask_smooth=s, mask_buffer=b,
      path_l1c = safe_dir,
      path_l2a = safe_dir,
      path_out = out_dir,
      # tmpdir = file.path(out_dir,"tmp2"),
      rmtmp=FALSE
    )
  }
}

# Load TCI images
maps <- data.frame(
  "map" = list.files(file.path(out_dir,"TCI"), "\\.tif$", full.names=TRUE),
  stringsAsFactors = FALSE
)
maps$smooth <- as.integer(gsub("^.+s([0-9]{3})b([0-9]{3}).+$","\\1",maps$map))
maps$buffer <- as.integer(gsub("^.+s([0-9]{3})b([0-9]{3}).+$","\\2",maps$map))
maps <- maps[maps$smooth %in% c(0,30,100) & maps$buffer %in% c(0,30,100),]
maps$n <- seq_len(nrow(maps))

# Plot them
plot_maps <- ggplot(maps, aes(x=0, y=0)) + 
  geom_image(aes(image=map), size=Inf) +
  geom_text(x=Inf, y=-Inf, aes(label = n), colour="white", hjust=2, vjust=-1, fontface=2) +
  facet_grid(smooth~buffer, switch="both") +
  # scale_size_identity() +
  # coord_fixed() +
  xlab("Buffer (m)") + ylab("Smooth (m)") + 
  theme_minimal() +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

# Export plots
jpeg(file.path(system.file("www","images", package="sen2r"),"mask_types.jpg"), 1450,850, res=100)
print(plot_maps)
dev.off()
