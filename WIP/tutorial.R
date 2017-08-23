
# Draft for a tutorial of the package: for now it is a list of instructions
# which can be runned sequentially to reproduce the package functions.

## 0) Define paths
base_path <- "/home/lranghetti/nas-s4a/nr_working/luigi/data/s2tsp/fidolasen_tutorial"
l1c_path <- file.path(base_path, "l1c")
l2a_path <- file.path(base_path, "l2a")


## 1) List some products in a quite extended area
#     (enough to include multiple tiles and orbits)
ita_boundaries <- sprawl::get_boundaries("Italy", 1)
ex_boundaries <- ita_boundaries[ita_boundaries$NAME_1=="Valle d'Aosta",]
plot(ex_boundaries)
time_window <- as.Date(c("2017-04-16","2017-05-31"))
example_s2_list <- s2_list(spatial_extent=ex_boundaries, time_interval=time_window)
print(example_s2_list)

## 2) Download them
s2_levels <- unlist(sapply(names(example_s2_list), s2_getMetadata, "nameinfo")["level",])
example_s2_list_l1c <- example_s2_list[s2_levels=="1C"]
example_s2_list_l2a <- example_s2_list[s2_levels=="2A"]

s2_download(example_s2_list_l1c, out_dir=l1c_path)
s2_sen2cor(names(example_s2_list_l1c), l1c_dir=l1c_path, out_dir=l2a_path, n_procs=4)
s2_download(example_s2_list_l2a, out_dir=l2a_path)
