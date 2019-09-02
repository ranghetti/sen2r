#' @title Create the database of S2 orbits and doy of passage
#' @description The internal function build a database with the base DOY of 
#'  passage across each Sentinel-2A orbit (which is used in function `s2_dop`).
#' @param json_path (optional) The path of the output JSON file.
#'  *Warning*: to create a file which will be usable by the package,
#'  this option must be left to NA (default location is within the
#'  package installation). Edit this only to create the file in another
#'  place for external use.
#' @param force (optional) Logical: if FALSE (default), the db is created only
#'  if missing or not updated; if TRUE, it is created in any case.
#' @return The path of the json file
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom utils download.file packageVersion


create_s2_dop <- function(json_path = NA, force = FALSE) {
  
  # to avoid NOTE on check
  . <- SENSING_TIME <- utm <- GRANULE_ID <- orbit <- PRODUCT_ID <- mission <- doybase <- NULL
  
  # check if the json already exists, and if the version is updated
  if (is.na(json_path)) {
    json_path <- file.path(system.file("extdata", package="sen2r"), "doybase.json")
  }
  if (system.file("extdata","doybase.json", package="sen2r") == json_path) {
    if (force == FALSE) {
      return(json_path)
    }
  }
  
  # Take products names from Google
  download.file(
    "https://storage.googleapis.com/gcp-public-data-sentinel-2/index.csv.gz",
    s2_store <- tempfile(fileext = ".gz")
  )
  s2_all <- fread(paste0("zcat < ", s2_store)) # quite long
  s2_all[,SENSING_TIME:=as.POSIXct(SENSING_TIME, format="%Y-%m-%dT%H:%M:%S")] # long
  
  # Extract data in a sufficient range of time
  s2_sel <- s2_all[SENSING_TIME>=as.POSIXct("2018-01-01") & SENSING_TIME<as.POSIXct("2018-01-11"),]
  s2_sel[,utm:=as.integer(substr(GRANULE_ID,6,7))]
  s2_sel[,orbit:=substr(PRODUCT_ID,35,37)]
  s2_sel[,mission:=substr(PRODUCT_ID,2,3)]
  
  # # Associate the timezone
  # utm_utc_list <- data.table(matrix(
  #   c(
  #     c( 1,-12),
  #     c( 2,-11), c( 3,-11), c( 4,-11),    c( 5,-10), c( 6,-10),
  #     c( 7, -9), c( 8, -9), c( 9, -9),    c(10, -8), c(11, -8),
  #     c(12, -7), c(13, -7), c(14, -7),    c(15, -6), c(16, -6),
  #     c(17, -5), c(18, -5), c(19, -5),    c(20, -4), c(21, -4),
  #     c(22, -3), c(23, -3), c(24, -3),    c(25, -2), c(26, -2),
  #     c(27, -1), c(28, -1), c(29, -1),    c(30,  0), c(31,  0),
  #     c(32,  1), c(33,  1), c(34,  1),    c(35,  2), c(36,  2),
  #     c(37,  3), c(38,  3), c(39,  3),    c(40,  4), c(41,  4),
  #     c(42,  5), c(43,  5), c(44,  5),    c(45,  6), c(46,  6),
  #     c(47,  7), c(48,  7), c(49,  7),    c(50,  8), c(51,  8),
  #     c(52,  9), c(53,  9), c(54,  9),    c(55, 10), c(56, 10),
  #     c(57, 11), c(58, 11), c(59, 11),    c(60, 12)
  #   ), ncol = 2, dimnames = list(NULL, c("utm", "utc")), byrow = TRUE
  # ))
  
  # # add local timedate
  # s2_sel[,utc:=utm_utc_list[match(s2_sel$utm, utm_utc_list$utm),"utc"]]
  # s2_sel[,local_datetime:=SENSING_TIME+3600*utc]
  
  # Compute doybase as the first digit of passage of S2A, 
  # with standard origin (1970-01-01)
  # (this is sufficient to compute dates of passage, since they are repeated 
  # with a period of 10 days)
  s2_sel[,date:=as.Date(strftime(SENSING_TIME, "%Y-%m-%d"))]
  s2_sel[,doybase:=as.integer(date)%%10]
  s2_sel[mission=="2B",doybase:=(as.integer(date)+5)%%10]
  
  # In case of datetime riding on midnight, the day after midnight is considered
  # (it corresponds to the day in local time)
  duplicated_orbit <- s2_sel[!duplicated(paste(orbit,doybase)),list(orbit,doybase)][duplicated(orbit),orbit]
  s2_sel[orbit %in% duplicated_orbit & as.integer(strftime(SENSING_TIME,"%H"))>12, date:=date+1]
  s2_sel[,doybase:=as.integer(date)%%10] # recompute after changing date
  s2_sel[mission=="2B",doybase:=(as.integer(date)+5)%%10]
  
  # Build database of orbits
  s2_dop_dt <- s2_sel[
    !duplicated(paste(doybase,orbit)),# & level=="1C",
    list(orbit,doybase)
    ][order(orbit),]
  
  # Export
  json_table <- list(
    "dop" = s2_dop_dt,
    "pkg_version" = as.character(packageVersion("sen2r")),
    "creation_date" = as.character(Sys.time())
  )
  writeLines(toJSON(json_table, pretty=TRUE), json_path)
  
  return(json_path)
  
}
