
# function to create maps of spectral indices
# from TOA / BOA images (create vrt if missing), load the index formula from a json
# and compute final map





# # WIP
#
# # Read HTML indices database from indexdatabase.de
# library(XML)
# s2_url <- "http://www.indexdatabase.de/db/is.php?sensor_id=96"
# download.file(s2_url, s2_path <- tempfile(), method = "wget", quiet = TRUE)
# s2_html <- htmlTreeParse(s2_path) %>%
#   xmlRoot()
# s2_html_table <- s2_html[["body"]][["div"]][["table"]]
# # doc <- htmlTreeParse(sub("s", "", fileURL), useInternal = TRUE)
#
# names(s2_html)
#
# # names_products <- names(xmlChildren(rootNode)) # names of the single products
# # names_products <- names_products[names_products != "comment"]
