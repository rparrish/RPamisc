#' SSI.
#'
#' A fake dataset containing the observed and expected 
#' risk of SSI.
#'
#' @format A data frame with 5975 rows and 5 variables:
#' \describe{
#'   \item{surgery_date}{date of surgery}
#'   \item{procCode}{NHSN Procedure Code: COLO = Colon surgery, HPRO = Hip Prosthesis, 
#'   KPRO = Knee Prosthesis, HYST = Abdominal Hysterectomy}
#'   \item{surgeon_name}{Fake Surgeon name (from `randomNames` package)}
#'   \item{observed}{Observed SSI. 1 = Yes, 0 = No}
#'   \item{expected}{Risk of SSI.}
#'   ...
#' }
"ssi"
