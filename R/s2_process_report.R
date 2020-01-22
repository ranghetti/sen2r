#' @title s2_process_report
#' @description Function use to summarize results of sen2r execution at the 
#'  end of a processing.
#' @param s2_list_ordered list containing the lists of ordered/notordered lta 
#'  S2 images
#' @param s2names PARAM_DESCRIPTION
#' @param pm list containing sen2r processing parameters
#' @param s2_list_cloudcovered s2_list_cloudcovered
#' @param s2_list_failed s2_list_failed
#' @param s2_list_cloud_ignored s2_list_cloudcovered
#' @param s2_list_failed_ignored s2_list_failed
#' @param download_only logical, if TRUE, indicates that the processing to be 
#'  summarized only involved download (pm$proeprocess = FALSE)
#' @return A data.frame summarizing the report, and containing the following columns: 
#'   time               = Date/time of report creation;  
#'   n_req_tot_dates    = Number of dates to be processed based on the query; 
#'   n_ondisk_dates     = Number of dates for which all expected products are already on disk; 
#'   n_proc_dates       = Number of date for which some products were computed in the 
#'    current run; 
#'   n_complete_out     = Number of dates for which processing is "complete", and all 
#'    products were created; 
#'   n_failed_dates     = Number of dates for which processing is "complete", but 
#'    some products were not created for unexpected reasons; 
#'   n_cloudy_dates     = Number of dates for which processing is "complete", but 
#'    some products were not created because cloudiness in the spatial extent was 
#'    above `max_mask`; 
#'   n_notonline_dates  = Number of date for which processing is "incomplete", 
#'    because not all require images are online;
#'   n_ordered_imgs     = Number of images correctly ordered from lta; 
#'   n_notordered_imgs  = Number of images for whic lta order failed; 
#'   completed          = Logical, indicating if processing can be considered "complete". 
#'    It is set to TRUE in case n_notonline_dates = 0. 
#' @rdname s2_process_report
#' 
s2_process_report <- function(s2_list_ordered,
                              s2names = NULL,
                              names_out = NULL, 
                              pm = NULL,
                              s2_list_cloudcovered   = NA, 
                              s2_list_failed         = NA, 
                              s2_list_cloud_ignored  = NA, 
                              s2_list_failed_ignored = NA, 
                              download_only = FALSE) {
    
    print_message(type = "message","\n ")
    print_message(
        type = "message",
        date = TRUE, 
        "###### sen2r Processing Report ###### ")
    browser()
    if (!download_only) {
        
        # First, compute number of non-processed and notonline DATES ----
        
        # n_notonline_dates: dates for which lta orders had to be placed, because 
        # some/all S2 images needed to compute them are offline (on LTA) AND 
        # not all required products are already on disk
        
        ordered_dates    <- as.Date(substr(names(s2_list_ordered), 12, 19), format = "%Y%m%d")
        notordered_dates <- as.Date(substr(names(attr(s2_list_ordered, "notordered"))
                                           , 12, 19), format = "%Y%m%d")
        notonline_dates   <- unique(c(ordered_dates, notordered_dates))
        n_notonline_dates <- length(notonline_dates)
        
        # Number of ordered/notordered IMAGES (NOT dates)
        n_ordered_imgs    <- length(ordered_dates)
        n_notordered_imgs <- length(notordered_dates)
        n_notonline_imgs  <- n_ordered_imgs + n_notordered_imgs
        
        # Then, verify which dates were not considered because of ignorelist/cloudlist
        # Here, we compute, for each date, the number of "failed" or cloudlisted
        # products 
        
        if (!all(is.na(s2_list_cloud_ignored))) {
            cloud_ignored_dates  <- as.Date(substr(basename(s2_list_cloud_ignored), 7, 14),
                                            format = "%Y%m%d")    
            lgt_cld_ignored <- table(cloud_ignored_dates)
        } else {
            lgt_cld_ignored <- 0
        }
        
        if (!all(is.na(s2_list_failed_ignored))) {
            failed_ignored_dates  <- as.Date(substr(basename(s2_list_failed_ignored), 7, 14),
                                             format = "%Y%m%d")    
            lgt_failed_ignored <- table(failed_ignored_dates)
        } else {
            lgt_failed_ignored <- 0
        }
        browser()
        if (lgt_cld_ignored != 0 | lgt_failed_ignored != 0) {
            lgt_existing     <- max(table(as.Date(substr(basename(unlist(s2names$exp)), 7, 14),
                                             format = "%Y%m%d")))
            n_cld_ignored    <- length(which(lgt_cld_ignored    == lgt_existing))
            n_failed_ignored <- length(which(lgt_failed_ignored == lgt_existing))
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
            n_ondisk_dates   <- n_expected_dates - 
                max(unlist(sapply(s2names$new, sapply, length)))    
            
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
            n_ondisk_dates   <- 0
            n_proc_dates     <- 0
            n_req_tot_dates  <- n_notonline_dates
        }
        
        #find the dates properly processed during current run: req_dates - notonline_dates
        if (!all(is.na(s2_list_failed))) {
            failed_dates <- unique(as.Date(substr(basename(s2_list_failed), 7, 14), 
                                           format = "%Y%m%d"))
        } else {
            failed_dates <- character(0)
        }
        
        if (!all(is.na(s2_list_cloudcovered))) {
            cloudy_dates <- unique(as.Date(substr(basename(s2_list_cloudcovered), 7, 14), 
                                           format = "%Y%m%d"))
        } else {
            cloudy_dates <- character(0)
        }
        
        # Dates for which no output created due to clouds or failure. Computed 
        # as the sum of failed/cloudy dates in current run, plus those eventually
        # failed in previous runs and stored in ignorelists
        n_failed_dates    <- length(failed_dates) + n_failed_ignored
        n_cloudy_dates    <- length(cloudy_dates) + n_cld_ignored
        
        # Issue messages ----
        
        # Issue message about number of "required" dates
        print_message(
            type = "message",
            date = TRUE, 
            "Dates to be processed based on processing parameters: ",
            n_req_tot_dates
        )
        
        # issue message about number of properly processed dates
        if (n_ondisk_dates != 0) {
            # Issue message about number of dates already ondisk
            print_message(
                type = "message",
                date = TRUE, 
                "All products already found on disk for: ", n_ondisk_dates, " dates. ", 
                "(Set `overwrite` to TRUE to reprocess)."
            )
        } 
        
        print_message(
            type = "message",
            date = TRUE, 
            "Processing completed for: ",
            ifelse(n_notonline_dates == 0, 
                   "all expected dates.", 
                   paste0(n_proc_dates + n_ondisk_dates, " out of: ",
                          n_req_tot_dates, " expected dates."))
        )
        
        # Report about MISSING dates
        if (n_failed_dates != 0) {
            print_message(
                type = "warning",
                date = TRUE, 
                paste0("Outputs for: ", n_failed_dates, 
                       " out of: ", n_req_tot_dates, " expected dates ", 
                       " not created because of unexpected reasons! \n")
            )
        }
        
        # Report about CLOUDY dates
        if (n_cloudy_dates != 0) {
            print_message(
                type = "message",
                date = TRUE, 
                "Outputs for: ", n_cloudy_dates, 
                " out of: ", n_req_tot_dates, " expected dates ", 
                " not created because cloudiness over the spatial extent is above ",
                pm$max_mask, "%.\n"
            )
        }
        
        # Report about NOTONLINE dates
        if (n_notonline_dates != 0) {
            print_message(
                type = "message",
                date = TRUE, 
                "Outputs for: ", n_notonline_dates, 
                " out of: ", n_req_tot_dates, " expected dates ", 
                "not created because all/some required images are currently offline."
            )
            
            # Report about ORDERED dates
            if (n_ordered_imgs != 0 && n_notordered_imgs == 0) {
                print_message(
                    type = "message",
                    date = TRUE, 
                    "Order for: ", n_ordered_imgs, " out of:  ",
                    n_notonline_imgs, " offline images successfull."
                )
                print_message(
                    type = "message",
                    date = TRUE, 
                    "You can check later if the ordered images are online ",
                    "with the command:\n",
                    ' safe_is_online("', attr(s2_list_ordered, "path"),'")\n'
                )
            }
            
            if (n_notordered_imgs != 0) {
                if (n_ordered_imgs != 0) {
                    print_message(
                        type = "message",
                        date = TRUE, 
                        "Order for: ", n_ordered_imgs, " out of: ",
                        n_notonline_imgs,
                        " offline images successfull."
                    )
                }
                print_message(
                    type = "message",
                    date = TRUE, 
                    "Order for: ", n_notordered_imgs, " out of: ",
                    n_notonline_imgs, 
                    " offline images failed due to exceeded user quota or other reasons.\n"
                )
                print_message(
                    type = "message",
                    date = TRUE,     
                    "You can try placing the order again later with the command:\n",
                    '  s2_order("',attr(s2_list_ordered, "path"),'")\n')
                print_message(
                    type = "message",
                    date = TRUE, 
                    "You can check later if the ordered images are online ",
                    "with the command:\n",
                    '  safe_is_online("',attr(s2_list_ordered, "path"),'")'
                )
            }
            print_message(
                type = "message",
                date = TRUE, 
                "When additional images are online, you can relaunch the processing chain ",
                "using the command:\n",
                '  sen2r("',attr(pm, "outpath"),'")'
            )
            
            print_message(
                type = "warning",
                date = TRUE, 
                "Processing was not completed because some required images are offline! ",
                "See the processing log!"
            )
            
        }
        
        print_message(type = "message", "\n")
        
        print_message(
            type = "message",
            date = TRUE,
            "###### Execution of sen2r session terminated ######",
            if (length(nn(s2_list_ordered)) == 0) {paste0(
                "\nThe processing chain can be re-launched with the command:\n",
                '  sen2r("',attr(pm, "outpath"),'")'
            )}
        )
        
        # create "status" data.frame    
        
        
        # a RUN is "complete" if we did not need to order images because all was online
        # This allows to easily plan "reprocessing" using a while cycle
        is_complete <- ifelse((n_ordered_imgs + n_notordered_imgs )== 0, 
                              TRUE, 
                              FALSE)
        
        status <- data.frame(
            time              = Sys.time(), 
            n_req_tot_dates   = n_req_tot_dates, 
            n_ondisk_dates    = n_ondisk_dates, 
            n_proc_dates      = n_proc_dates, 
            n_complete_out    = n_proc_dates +  n_ondisk_dates - n_failed_dates - n_cloudy_dates,
            n_failed_dates    = n_failed_dates, 
            n_cloudy_dates    = n_cloudy_dates, 
            n_notonline_dates = n_notonline_dates, 
            n_ordered_imgs    = n_ordered_imgs, 
            n_notordered_imgs = n_notordered_imgs, 
            completed         = is_complete)   
    }
}
