#' bg_window
#'
#' Get the BG data from the 18-24 hour post-op window,
#'
#' returns the start & end time of the BG window, the minimum & maximum
#' BG values, the total number of values and the number over 180.
#'
#' @param cp_id CommunityPatientID
#' @param surgery_date The date of surgery in ISO format
#' @param surgery_end_time  The surgery end time in HH:MM
#' @return Returns a data frame with cp_id, start,end (of the BG Window) min, max, count_values,
#' & count_180.
#'
#' @keywords bg
#' @examples
#' ## single
#' cp_id <- c("1501668269")
#' surgery_date <- "2014-10-02"
#' surgery_end_time <- "13:22"
#'
#' bg_window(cp_id, surgery_date, surgery_end_time)
#'
#' ## multiples
#' cases <- data.frame(cp_id = c("1504685006","1503010730", "1502850183"),
#'                     surgery_date = c("2014-09-25", "2014-10-01", "2014-09-30"),
#'                     surgery_end_time = c("16:58", "16:49", "13:40"))
#'
#' bg_window_data <- apply(cases[, c('cp_id', 'surgery_date', 'surgery_end_time')], 1,
#'                      function(y) bg_window(y['cp_id'], y['surgery_date'], y['surgery_end_time']))
#'
#' do.call(rbind, bg_window_data)
#'
#' @export
#'




bg_window <- function(cp_id, surgery_date, surgery_end_time) {

    ## bg_window_sql file is located in RPamisc/inst/sql folder
    if(!exists(".bg_window_sql")) {
        .bg_window_sql <-
            readLines( system.file("sql", "bg_window.sql", package="RPamisc"),
                       ok = TRUE, warn = FALSE)
    }
    sql <- sprintf(.bg_window_sql, cp_id, sep="")

    mydata <- edwSQL(sql=sql, resource="Trantor", file=FALSE)$data

    surgery_end_datetime <- as.POSIXct(paste(surgery_date, surgery_end_time))

    bg_window_start <- surgery_end_datetime + 18*60*60
    bg_window_end <- surgery_end_datetime + 24*60*60

    values <- mydata[!is.na(mydata$ObtainedDTS)
                     & mydata$ObtainedDTS >= bg_window_start
                     & mydata$ObtainedDTS <= bg_window_end
                     & mydata$CommunityPatientID == cp_id, ]

    minimum <- suppressWarnings(min(values$ResultValueNBR))
    maximum <- suppressWarnings(max(values$ResultValueNBR))
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
