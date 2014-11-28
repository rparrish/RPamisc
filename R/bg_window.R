#' bg_window
#'
#' Get the BG data from the 18-24 hour post-op window,
#'
#' returns the window start & end time, min & max BG values,
#' the number of values and the number over 180 for a
#' CommunityPatientID with a given surgery date and surgery end time.
#'
#' @param cp_id CommunityPatientID
#' @param surgery_date The date of surgery in ISO format
#' @param surgery_end_time  The surgery end time in HH:MM
#' @return Data frame with cp_id  cp_id, start,end (of the BG Window) min, max, count_values, count_180
#'
#' @keywords sql
#' @export
#'




bg_window <- function(cp_id, surgery_date, surgery_end_time) {

    ## bg_window_sql file is located in RPamisc/inst/sql folder
    sql <- sprintf(.bg_window_sql, cp_id, sep="")

    mydata <- edwSQL(sql=sql, resource="Trantor", file=FALSE)$data

    surgery_end_datetime <- as.POSIXct(paste(surgery_date, surgery_end_time))

    bg_window_start <- surgery_end_datetime + 18*60*60
    bg_window_end <- surgery_end_datetime + 24*60*60

    values <- mydata[!is.na(mydata$ObtainedDTS)
                     & mydata$ObtainedDTS >= bg_window_start
                     & mydata$ObtainedDTS <= bg_window_end
                     & mydata$CommunityPatientID == cp_id, ]

    minimum <- min(values$ResultValueNBR)
    maximum <- max(values$ResultValueNBR)
    count_values <- length(values$OrderProcedureID)
    count_180 <- length(values[which(values$ResultValueNBR > 180)])

    results <- data.frame(cp_id = cp_id,
                          start = bg_window_start,
                          end = bg_window_end,
                          min = minimum,
                          max = maximum,
                          #values = values,
                          count_values = count_values,
                          count_180 = count_180
    )
    return(results)
}
