#' medd
#'
#' converts the 24hr total dosages of various opioid medications and routes
#' into a Morphine Equivalent Daily Dose (MEDD)
#' @param fentanyl_iv IV fentaNYL total
#' @param fentanyl_td transdermal fentaNYL total
#' @param hydrocodone_po PO hydrocodone  total
#' @param hydromorphone_iv IV hydromorphone total
#' @param hydromorphone_po PO hydromorphone total
#' @param methadone_iv methadone  total
#' @param morphine_iv IV morphine  total
#'
#' @param oxycodone_po PO Oxycodone total
#' @param oxymorphone_po PO Oxymorphone total
#' @param sufentanil_iv IV sufentanil
#' @param tramadol_iv IV Tramadol
#'
#' @keywords medd
#' @return a list of the total MEDD values
#' @export
#' @examples
#' data <- data.frame(names=c("john", "jane", "peter"),
#'                  hydrocod_po=c(5,10,NA),
#'                  morphine_iv=c(0,10,1),
#'                  hydromorphone_iv = c(1,2,3),
#'                  fentanyl_iv = c(2,4,6),
#'                  fentanyl_td = c(1,1,1),
#'                  hydromorphone_po = c(NA, NA, 2),
#'                  methadone_iv = c(1,2,3),
#'                  oxycodone_po = c(NA, NA, 1),
#'                  oxymorphone_po = c(2,4,6),
#'                  sufentanil_iv = c(2,3,4),
#'                  tramadol_iv = c(1,NA,2)
#'                   )
#' data[is.na(data)] <- 0
#' data$total <- mapply(medd,
#'      hydrocodone_po = data$hydrocod_po,
#'      morphine_iv = data$morphine_iv,
#'      hydromorphone_iv = data$hydromorphone_iv,
#'      fentanyl_iv = data$fentanyl_iv,
#'      fentanyl_td = data$fentanyl_td,
#'      hydromorphone_po = data$hydromorphone_po,
#'      methadone_iv = data$methadone_iv,
#'      oxycodone_po = data$oxycodone_po,
#'      oxymorphone_po = data$oxymorphone_po,
#'      sufentanil_iv = data$sufentanil_iv,
#'      tramadol_iv = data$tramadol_iv
#'      )
#'
#' as.data.frame(t(data))


medd <- function (hydrocodone_po=0,
                   morphine_iv=0,
                   hydromorphone_iv=0,
                   fentanyl_iv = 0,
                   fentanyl_td = 0,
                   hydromorphone_po = 0,
                   methadone_iv = 0,
                   oxycodone_po = 0,
                   oxymorphone_po = 0,
                   sufentanil_iv = 0,
                   tramadol_iv = 0
                  ) {

    meq.hydrocodone_po = 3
    meq.morphine_iv = 11
    meq.hydromorphone_iv = 5
    meq.fentanyl_iv = 10
    meq.fentanyl_td = 1
    meq.hydromorphone_po = 7
    meq.methadone_iv = 9
    meq.oxycodone_po = 13
    meq.oxymorphone_po = 15
    meq.sufentanil_iv = 17
    meq.tramadol_iv = 18

    total <- prod(hydrocodone_po, meq.hydrocodone_po, na.rm = TRUE)  +
        prod(morphine_iv, meq.morphine_iv, na.rm = TRUE) +
        prod(hydromorphone_iv, meq.hydromorphone_iv, na.rm = TRUE) +
        prod(fentanyl_iv, meq.fentanyl_iv, na.rm = TRUE) +
        prod(fentanyl_td, meq.fentanyl_td, na.rm = TRUE) +
        prod(hydromorphone_po, meq.hydromorphone_po, na.rm = TRUE) +
        prod(methadone_iv, meq.methadone_iv, na.rm = TRUE) +
        prod(oxycodone_po, meq.oxycodone_po, na.rm = TRUE) +
        prod(oxymorphone_po, meq.oxymorphone_po, na.rm = TRUE) +
        prod(sufentanil_iv, meq.sufentanil_iv, na.rm = TRUE) +
        prod(tramadol_iv, meq.tramadol_iv, na.rm = TRUE)

    return (total)
}
