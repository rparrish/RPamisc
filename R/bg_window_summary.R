
#' bg_window_summary
#'
#' wrapper function for bg_window,
#'
#' takes a data frame with columns cp_id, date_surgery, and surgery_end_dts
#' and returns the bg_window data for each row
#'
#' @param cases data.frame that includes column names "cp_id", "date_surgery", "surgery_end_dts"
#' @return Returns a data frame with cp_id, start,end (of the BG Window) min, max, count_values,
#' & count_180.
#'
#' @keywords bg
#' @export


bg_window_summary <- function(cases) {

    ## check for required column names

    mydata <- apply(cases[, c('cp_id', 'date_surgery', 'surgery_end_dts')], 1,
                    function(y) {
                        bg_window(y['cp_id'], y['date_surgery'], y['surgery_end_dts'])
                    })

    mydata_final <- do.call(rbind, mydata)
    results <- do.call(data.frame,lapply(mydata_final, function(x) replace(x, is.infinite(x),NA)))
    return(results)
}
