#' @title Summarise processing report
#' @description Internal function used to summarise results of sen2r execution
#'  at the end of a processing..
#' @param s2_list_ordered List containing the lists of ordered/notordered LTA
#'  S2 images.
#' @param s2names Output of [compute_s2_paths()].
#' @param pm List containing sen2r processing parameters.
#' @param ignorelist Internal parameter.
#' @param s2_list_cloudcovered Internal parameter.
#' @param s2_list_failed Internal parameter.
#' @param download_only Logical: if TRUE, it indicates that the processing to be 
#'  summarised only involved download (`pm$preprocess = FALSE`).
#' @param s2_downloaded Internal parameter.
#' @param s2_skipped Internal parameter.
#' @param s2_corrected Internal parameter.
#' @return A data.frame summarising the report, and containing the following columns: 
#'   - `time`: date/time of report creation;
#'   - `n_req_tot_dates`: number of dates to be processed based on the query; 
#'   - `n_ondisk_dates`: number of dates for which all expected products are 
#'       already on disk; 
#'   - `n_proc_dates`: number of date for which some products were computed in
#'       the current run; 
#'   - `n_complete_out`: number of dates for which processing is "complete", 
#'       and all products were created; 
#'   - `n_failed_dates`: number of dates for which processing is "complete", 
#'       but some products were not created for unexpected reasons; 
#'   - `n_cloudy_dates`: number of dates for which processing is "complete", 
#'       but some products were not created because cloudiness in the spatial 
#'       extent was above `max_mask`; 
#'   - `n_notonline_dates`: number of date for which processing is "incomplete", 
#'       because not all require images are online;
#'   - `n_ordered_imgs`: number of images correctly ordered from LTA; 
#'   - `n_notordered_imgs`: number of images for which LTA order failed; 
#'   - `n_downloaded`: number of images downloaded during current run; 
#'   - `n_skipped`: number of required images not downloaded because already 
#'       on disk
#'       (note: this does not include images that would be needed to process 
#'       a date for which all products are already on disk);
#'   - `n_corrected`: number of images atmospherically corrected using sen2cor; 
#'   - `completed`: logical, indicating if processing can be considered
#'       "complete" (it is set to TRUE in case `n_notonline_dates = 0`). 
#' @author Lorenzo Busetto, phD (2020) \email{lbusett@@gmail.com}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing 
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI: 
#'  \href{https://doi.org/10.1016/j.cageo.2020.104473}{10.1016/j.cageo.2020.104473}, 
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0


sen2r_process_report <- function(
  s2_list_ordered,
  s2names = NULL,
  pm = NULL,
  ignorelist = list(),
  s2_list_cloudcovered = NA, 
  s2_list_failed = NA, 
  download_only = FALSE, 
  s2_downloaded = NA, 
  s2_skipped = NA, 
  s2_corrected = NA
) {
  
  # print_message(type = "message","\n ")
  print_message(
    type = "message",
    "\u2554",rep("\u2550", 0.9 * getOption("width") - 2),"\n",
    "\u2551 sen2r Processing Report\n",
    "\u255f",rep("\u2500", 0.9 * getOption("width") - 2)
  )
  
  # First, compute number of non-processed and notonline DATES ----
  
  # n_notonline_dates: dates for which lta orders had to be placed, because 
  # some/all S2 images needed to compute them are offline (on LTA) AND 
  # not all required products are already on disk
  
  ordered_dates <- as.Date(substr(names(s2_list_ordered), 12, 19), format = "%Y%m%d")
  notordered_dates <- as.Date(
    substr(names(attr(s2_list_ordered, "notordered")), 12, 19), 
    format = "%Y%m%d"
  )
  notonline_dates <- unique(c(ordered_dates, notordered_dates))
  n_notonline_dates <- length(notonline_dates)
  
  # Number of ordered/notordered IMAGES (NOT dates)
  n_ordered_imgs <- length(ordered_dates)
  n_notordered_imgs <- length(notordered_dates)
  n_notonline_imgs <- n_ordered_imgs + n_notordered_imgs
  
  if (download_only == FALSE) {
    
    # Then, verify which dates were not considered because of ignorelist/cloudlist
    # Here, we compute, for each date, the number of "failed" or cloudlisted
    # products 
    cloud_ignored_dates <- ignorelist$dates_cloudcovered
    s2_list_failed_ignored <- ignorelist$names_missing
    
    if (!all(is.na(cloud_ignored_dates))) {
      cloud_ignored_dates <- as.Date(character(0))
    }
    tbl_cld_ignored <- if (length(cloud_ignored_dates) > 0) {
      table(cloud_ignored_dates)
    } else {
      0
    }
    
    if (!all(is.na(s2_list_failed_ignored))) {
      s2_list_failed_ignored <- character(0)
    }
    failed_ignored_dates <- sort(unique(
      sen2r_getElements(s2_list_failed_ignored)$sensing_date
    ))
    tbl_failed_ignored <- if (length(s2_list_failed_ignored) > 0) {
      table(failed_ignored_dates)
    } else {
      0
    }
    
    if (all(
      length(tbl_cld_ignored) != 0 | length(tbl_failed_ignored) != 0,
      !is.null(s2names)
    )) {
      exp_new_files <- s2names$exp[!attr(s2names, "paths_istemp")[names(s2names$exp)]]
      
      dates_expected <- sort(unique(
        sen2r_getElements(unlist(exp_new_files))$sensing_date
      ))
      
      if (length(dates_expected) != 0) {
        lgt_expected <- max(table(dates_expected))
      } else {
        lgt_expected <- 0
      }
      # check to get correct report if all images are skipped because
      # of ignorelist
      if (lgt_expected != 0) {
        n_cld_ignored <- length(which(tbl_cld_ignored == lgt_expected))
        n_failed_ignored <- length(which(tbl_failed_ignored == lgt_expected))
      } else {
        n_cld_ignored    <- length(cloud_ignored_dates)
        n_failed_ignored <- length(failed_ignored_dates)
      }
    } else {
      n_cld_ignored <- n_failed_ignored <- 0
    }
    
    # Then, verify which dates where processed
    
    if (!is.null(s2names)) {
      # n_expected_dates: computed as the maximum number of expected files
      # for a product, plus the number of dates skipped because of ignorelists
      n_expected_dates <- max(unlist(sapply(s2names$exp, sapply, length))) +
        n_cld_ignored + n_failed_ignored
      
      # n_ondisk_dates: computed as n_expected_dates minus the maximum number 
      # of new files to be created for a product. 
      n_ondisk_dates <- n_expected_dates - 
        max(unlist(sapply(s2names$new, sapply, length))) -    
        n_cld_ignored - n_failed_ignored
      
      # n_proc_dates = dates for which SOME processing had to be made in 
      # current "run". Computed as the difference. Should be equivalent
      # to max(unlist(sapply(s2names$new, sapply, length))) 
      n_proc_dates <- n_expected_dates - n_ondisk_dates
      
      # n_req_tot_dates: total dates to be processed based on the query. 
      # Computed as the sum of dates for which data is available online
      # and dates for which lta orders where placed
      n_req_tot_dates <- n_expected_dates + n_notonline_dates
    } else {
      # if s2names is null, we are calling it from a processing 
      # where no dates are already on disk OR have full data online!
      n_expected_dates <- length(s2_list_ordered) + 
        length(attr(s2_list_ordered, "notordered"))
      n_ondisk_dates <- 0
      n_proc_dates <- 0
      n_req_tot_dates <- n_notonline_dates
    }
    
    #find the dates properly processed during current run: req_dates - notonline_dates
    if (!all(is.na(s2_list_failed))) {
      failed_dates <- sort(unique(
        sen2r_getElements(s2_list_failed)$sensing_date
      ))
    } else {
      failed_dates <- character(0)
    }
    
    if (!all(is.na(s2_list_cloudcovered))) {
      cloudy_dates <- sort(unique(
        sen2r_getElements(s2_list_cloudcovered)$sensing_date
      ))
    } else {
      cloudy_dates <- character(0)
    }
    
    # Dates for which no output created due to clouds or failure. Computed 
    # as the sum of failed/cloudy dates in current run, plus those eventually
    # failed in previous runs and stored in ignorelists
    n_failed_dates <- length(failed_dates) + n_failed_ignored
    n_cloudy_dates <- length(cloudy_dates) + n_cld_ignored
    
    # Issue messages ----
    
    # Issue message about number of "required" dates
    print_message(
      type = "message",
      prefix = "\u2551 ", 
      "Dates to be processed based on processing parameters: ",
      n_req_tot_dates
    )
    
    # issue message about number of properly processed dates
    if (n_ondisk_dates != 0) {
      # Issue message about number of dates already ondisk
      print_message(
        type = "message",
        prefix = "\u2551 ", 
        "All products already found on disk for: ", n_ondisk_dates,
        " dates (set \"overwrite\" to TRUE to reprocess)."
      )
    } 
    
    print_message(
      type = "message",
      prefix = "\u2551 ", 
      "Processing completed for: ",
      ifelse(
        all(n_notonline_dates == 0, n_failed_dates == 0),
        "all expected dates.", 
        paste0(
          n_proc_dates + n_ondisk_dates - n_failed_dates, " out of ", 
          n_req_tot_dates, " expected dates."
        )
      )
    )
    
    # Report about MISSING dates
    if (n_failed_dates != 0) {
      print_message(
        type = "message",
        prefix = "\u2551 ", 
        "WARNING: Outputs for: ", n_failed_dates, 
        " out of ", n_req_tot_dates, " expected dates ", 
        " not created because of unexpected reasons.",
        "\"\nThese files will be skipped during next executions ",
        "from the current JSON parameter file. ",
        "To try again to build them, remove their file names in the text file \"",
        path_ignorelist(pm),"\"."
      )
    }
    
    # Report about CLOUDY dates
    if (n_cloudy_dates != 0) {
      print_message(
        type = "message",
        prefix = "\u2551 ", 
        "Outputs for: ", n_cloudy_dates, 
        " out of ", n_req_tot_dates, " expected dates ", 
        " not created because cloudiness over the spatial extent is above ",
        pm$max_mask, "%.",
        "\n\t The list of these files was written in a hidden file, ",
        "so to be skipped during next executions from the current ",
        "JSON parameter file. \n",
        "To process them again (e.g., because you changed the \"max_mask\" setting) ",
        "delete their dates in the text file \"",path_ignorelist(pm),"\". "
      )
    }
    
    # Report about NOTONLINE dates
    if (n_notonline_dates != 0) {
      print_message(
        type = "message",
        prefix = "\u2551 ", 
        "Outputs for: ", n_notonline_dates, 
        " out of ", n_req_tot_dates, " expected dates ", 
        "not created because all/some required images are currently offline."
      )
      
      # Report about ORDERED dates
      if (n_ordered_imgs != 0 && n_notordered_imgs == 0) {
        print_message(
          type = "message",
          prefix = "\u2551 ", 
          "Order for: ", n_ordered_imgs, " out of ",
          n_notonline_imgs, " offline images successfull."
        )
        print_message(
          type = "message",
          prefix = "\u2551 ", 
          "You can check later if the ordered images are online ",
          "with the command:\n",
          '\u00A0\u00A0safe_is_online("', attr(s2_list_ordered, "path"),'")'
        )
      }
      
      if (n_notordered_imgs != 0) {
        if (n_ordered_imgs != 0) {
          print_message(
            type = "message",
            prefix = "\u2551 ", 
            "Order for: ", n_ordered_imgs, " out of ",
            n_notonline_imgs, 
            " offline images successfull."
          )
        }
        print_message(
          type = "message",
          prefix = "\u2551 ", 
          "Order for: ", n_notordered_imgs, " out of ",
          n_notonline_imgs, 
          " offline images failed due to exceeded user quota or other reasons."
        )
        print_message(
          type = "message",
          prefix = "\u2551 ", 
          "You can try placing the order again later with the command:\n",
          '\u00A0\u00A0s2_order("',attr(s2_list_ordered, "path"),'")'
        )
        print_message(
          type = "message",
          prefix = "\u2551 ", 
          "You can check later if the ordered images are with the command:\n",
          '\u00A0\u00A0safe_is_online("',attr(s2_list_ordered, "path"),'")'
        )
      }
      print_message(
        type = "message",
        prefix = "\u2551 ", 
        "When additional images are online, you can relaunch the processing chain ",
        "using the command:\n",
        '\u00A0\u00A0sen2r("',attr(pm, "outpath"),'")'
      )
      
      print_message(
        type = "warning",
        "Processing was not completed because some required images are offline ",
        "(see the processing log)."
      )
      
    }
    # number of dates for which all expected processing is finished    
    n_complete_out <- n_proc_dates +  n_ondisk_dates - n_failed_dates - n_cloudy_dates
  } else {
    # if only download, only report the number of downloades/skipped/ordered/notordered
    # images (could change in the future!)
    n_req_tot_dates <- NA
    n_proc_dates    <- NA
    n_complete_out  <- NA
    n_failed_dates  <- NA
    n_cloudy_dates  <- NA
    n_ondisk_dates  <- NA
    # number of dates for which all expected processing is finished    
    n_complete_out <- n_proc_dates +  n_ondisk_dates
  }
  
  # get number of downloaded images
  if (!all(is.na(s2_downloaded))) {
    n_downloaded <- length(which(!is.na(s2_downloaded)))
  } else {
    n_downloaded <- 0
  }
  
  if (!all(is.na(s2_skipped))) {
    n_skipped <- length(which(!is.na(s2_skipped)))
  } else {
    n_skipped <- 0
  }
  
  if (!all(is.na(s2_corrected))) {
    n_corrected <- length(which(!is.na(s2_corrected)))
  } else {
    n_corrected <- 0
  }
  
  
  # a RUN is "complete" if we did not need to order images because all was online
  # This allows to easily plan "reprocessing" using a while cycle
  is_complete <- (n_ordered_imgs + n_notordered_imgs ) == 0
  
  # create "status" data.frame    
  status <- data.frame(
    time              = Sys.time(), 
    n_req_tot_dates   = n_req_tot_dates, 
    n_ondisk_dates    = n_ondisk_dates, 
    n_proc_dates      = n_proc_dates, 
    n_complete_out    = n_complete_out,
    n_failed_dates    = n_failed_dates, 
    n_cloudy_dates    = n_cloudy_dates, 
    n_notonline_dates = n_notonline_dates, 
    n_ordered_imgs    = n_ordered_imgs, 
    n_notordered_imgs = n_notordered_imgs, 
    n_downloaded      = n_downloaded,
    n_skipped         = n_skipped,
    n_corrected       = n_corrected,
    completed         = is_complete
  )
  
  print_message(
    type = "message",
    "\u255a",rep("\u2550", 0.9 * getOption("width") - 2),"\n"
  )
  print_message(
    type = "message",
    date = TRUE,
    "#### sen2r session terminated. ####"
  )
  
  if (length(nn(s2_list_ordered)) == 0) {
    print_message(
      type = "message",
      date = FALSE,
      "The processing chain can be re-launched with the command:\n",
      '\u00A0\u00A0sen2r("',attr(pm, "outpath"),'")'
    )
  }
  return(status)
}
