#' @title Return the Dates Of Passage over some orbits
#' @description The function allows to know which Sentinel-2 passages should
#'  pass over certain orbits during a defined time interval.
#'  Dates are intended to be in UTC time.
#'  Notice that this is the expected calendar: some unexpected events
#'  (e.g. techical problems, or early working phases during first stages of
#'  acquisition) could cause the data unavailability even if an 
#'  acquisition was expected.
#'  Notice also that some orbits (030, 073 and 116) acquire across UTC midnight:
#'  in this cases, the date is assumed to be the one of the acquisition after 
#'  midnight (which corresponds to the date in local time).
#' @param s2_orbits A vector of Sentinel-2 orbits (as integer numbers 
#'  or 3-length character).
#'  Default is all the 143 orbits.
#' @param timewindow Temporal window for querying: Date object
#'  of length 1 (single day) or 2 (time window). 
#'  Is it possible to pass also integer (or difftime) values, which are 
#'  interpreted as the next n days (if positive) or the past n days 
#'  (if negative).
#'  Also strings which can be interpreted as time ranges are accepted 
#'  (see examples).
#'  Default is the next 10 days (one cycle).
#' @param mission (optional) Vector with the desired Sentinel-2 missions
#'  ("2A", "2B" or both). Default is both.
#' @return A data table with the dates (column "date"), the missions 
#' (column "mission") and the orbits (column "orbit").
#' An empty data table with the same structure is returned if no passages
#'  were found with the passed settings.
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
#' @import data.table
#' @importFrom stringr str_pad
#' @importFrom jsonlite fromJSON
#' @importFrom methods is
#' @export
#' 
#' @examples
#' # All the passages in a cycle of 10 days over all the orbits
#' s2_dop()
#' 
#' # The passages in the current month over two orbits
#' s2_dop(c("022", "065"), "this month")
#' 
#' # The dates in which Sentinel-2A will pass in next six weeks over one orbit 
#' s2_dop("022", "6 weeks", mission = "2A")$date
#' 
#' # The date in which Sentinel-2A would be passed in the last 10 days over one orbit 
#' s2_dop("022", "-10 days", mission = "2A")$date
#' 
#' # All the orbits covered today
#' s2_dop(timewindow = Sys.Date(), mission = "2B")$orbit
#' 
#' # The passages in a fixed time window for one orbit
#' s2_dop(65, as.Date(c("2018-08-01", "2018-08-31")))
#' 
#' # A research with no passages found
#' s2_dop(22, "2018-08-16", mission = "2A")


s2_dop <- function(s2_orbits = 1:143, 
                   timewindow = "10 days", 
                   mission = c("2A", "2B")) {
  
  # to avoid NOTE on check
  type <- orbit <- doybase <- orbit <- NULL
  
  # generate doybase.json if missing
  json_path <- create_s2_dop()
  s2_dop_dt <- data.table(jsonlite::fromJSON(json_path)$dop)
  
  ## Check the length of arguments
  if (length(s2_orbits) == 0 | length(timewindow) == 0 | length(mission) == 0) {
    return(
      data.table(
        "date" = as.Date(character(0)),
        "mission" = character(0),
        "orbit" = character(0), 
        stringsAsFactors = FALSE
      )
    )
  }
  
  ## Check s2_orbits
  if (is.numeric(s2_orbits)) {
    s2_orbits <- str_pad(as.integer(s2_orbits), 3, "left", "0")
  }
  
  ## Check mission
  if (!all(grepl("^2[AB]$", mission))) {
    print_message(
      type = "error",
      "Parameter 'mission' cannot contain values different from \"2A\" and/or \"2B\"."
    )
  }
  
  ## Check timewindow
  # check that it does not contain NAs
  if (anyNA(timewindow)) {
    print_message(
      type = "error",
      "Parameter 'timewindow' cannot contain NA values."
    )
  }
  
  # Check to be 1 or 2 length
  if (length(timewindow)>2) {
    print_message(
      type = type,
      "Parameter 'timewindow' must be of length 1 or 2."
    )
  }
  
  # If difftime, threat as character
  if (is(timewindow, "difftime")) {
    timewindow <- paste(timewindow, attr(timewindow, "units"))
  }
  
  # If 1-length numeric, threat as difftime in days
  if (length(timewindow)==1 & is(timewindow, "numeric")) {
    timewindow <- as.character(as.integer(timewindow))
  }
  
  # Check character input (to be a date or a time period)
  if (is(timewindow, "character")) {
    # if it is a date, convert in Date
    try_date <- tryCatch({timewindow <- as.Date(timewindow)}, error = function(e){"error"})
    # otherwise, check if it is a recognised difftime string
    if (is.character(try_date)) {
      timewindow <- strsplit(timewindow, " ")[[1]]
      if (timewindow[1]=="this") {
        timewindow[1] <- 1
      }
      if (timewindow[1]=="next") {
        timewindow <- timewindow[-1]
      }
      if (timewindow[1] %in% c("past","last")) {
        timewindow <- timewindow[-1]
        timewindow[1] <- paste0("-",timewindow[1])
      }
      if (
        any(!grepl(
          "^((\\-?[0-9]+)|(this)|(days?)|(weeks?)|(months?)|(years?))$", 
          tolower(timewindow)
        )) |
        length(timewindow) > 2
      ) {
        print_message(
          type = "error",
          "Parameter 'timewindow' is not a recognised string."
        )
      }
      if (length(timewindow)==1) {
        if (grepl("^((\\-?[0-9]+)|(this))$", tolower(timewindow))) {
          timewindow <- c(timewindow, "days")
        } else {
          timewindow <- c(1, timewindow)
        }
      }
      timewindow_start <- switch(
        tolower(gsub("s$","",timewindow[2])),
        day = Sys.Date(),
        week = as.Date(cut(Sys.Date(), "week")),
        month = as.Date(strftime(Sys.Date(),"%Y-%m-01")),
        year = as.Date(strftime(Sys.Date(),"%Y-01-01"))
      )
      timewindow_all <- if (as.integer(timewindow[1]) > 0) {
        seq(timewindow_start, length=as.integer(timewindow[1])+1, by=timewindow[2])
      } else {
        sort(c(
          seq(timewindow_start, length=2, by=timewindow[2])[2],
          seq(timewindow_start, length=-as.integer(timewindow[1]), by=paste("-1",timewindow[2]))
        ))
      }
      timewindow <- timewindow_all[c(1,length(timewindow_all))] + c(0,-1)
    }
  }
  
  # If it is a POSIXct, convert in Date
  if (is(timewindow, "POSIXt")) {
    timewindow <- as.Date(strftime(timewindow))
  }
  
  # If it is a 1-length Date, consider as 1-day interval
  if (length(timewindow)==1 & is(timewindow, "Date")) {
    timewindow <- rep(timewindow, 2)
  }
  
  # If conversions in Date were not sufficient, stop
  if (!is(timewindow, "Date")) {
    print_message(
      type = type,
      "Parameter 'timewindow' is not in a recognised format."
    )
  }
  
  dates_all <- seq(timewindow[1], timewindow[2], by = "day")
  
  
  ## Compute the dates 
  sel_dop_dt <- s2_dop_dt[orbit %in% s2_orbits,]
  s2a_dates <- if ("2A" %in% mission) {
    dates_all[(as.integer(dates_all)%%10) %in% sel_dop_dt$doybase]
  } else {
    as.Date(character(0))
  }
  s2b_dates <- if ("2B" %in% mission) {
    dates_all[(as.integer(dates_all)%%10) %in% ((sel_dop_dt$doybase+5)%%10)]
  } else {
    as.Date(character(0))
  }
  s2a_orbits <- lapply(s2a_dates, function(d) {sel_dop_dt[doybase==as.integer(d)%%10,orbit]})
  s2b_orbits <- lapply(s2b_dates, function(d) {sel_dop_dt[doybase==(as.integer(d)+5)%%10,orbit]})
  s2_missions <- c(rep("2A", length(s2a_dates)), rep("2B", length(s2b_dates)))
  s2_data <- rbindlist(lapply(seq_along(s2_missions), function(i) {
    expand.grid(
      "date" = c(s2a_dates, s2b_dates)[i], 
      "mission" = s2_missions[i], 
      "orbit" = c(s2a_orbits, s2b_orbits)[[i]], 
      stringsAsFactors = FALSE
    )
  }))
  
  if (nrow(s2_data) > 0) {
    # Order data
    setorder(s2_data, date, mission, orbit)
    # Remove unexisting records
    s2_data <- s2_data[
      mission == "2A" & date >= "2015-06-27" | 
        mission == "2B" & date >= "2017-06-29"
      ]
    return(s2_data)
  } else {
    return(
      data.table(
        "date" = as.Date(character(0)),
        "mission" = character(0),
        "orbit" = character(0), 
        stringsAsFactors = FALSE
      )
    )
  }
  
}