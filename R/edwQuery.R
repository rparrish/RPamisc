#' edwQuery
#'
#' edwQuery
#'
#' passes a SQL query to the EDW.
#'
#' @param sql - sql query
#' @param resource - Alias for the database connection
#'
#' \describe{
#'   \item{custom}{allows for a connection to any database server. The connection string must be passed using the custom parameter.}
#'   \item{EDWDBDev}{EDWDBDev}
#'   \item{EDWDBProd}{EDWDBProd}
#'   \item{Trantor}{EDWDBProd}
#'   \item{Phloston}{EDWDBDev}
#'   \item{ClinicalAnalytics}{\\\\wn2591\\PremierPRD}
#'   \item{Apollo}{Apollo_Spokane}
#'   \item{Eva}{\\\\wn1444 Testing}
#' }
#'
#' @param custom full connection string when using 'custom' as the resource.
#' @param DSN if TRUE, use a DSN as specified in `resource` parameter instead of a connection string (ie. for Axis PATS: resource = "TSI_32", DSN = TRUE)
#' @param uid username for connections if necessary. Default is NULL, which will then use the user's system credentials.
#' @param pwd password for connections that don't use the user's system credentials. Default is NULL.
#' @param ... arguments to be passed to RODBC::odbcDriverConnect
#' @return
#' \item{data}{Query results as a data frame}
#' \item{fields}{field names}
#' \item{elapsed_seconds}{elapsed system time to run the query}
#' \item{status_message}{the number of records & variables in the results}
#'
#' @keywords sql
#' @export
#'
#'
#' @examples
#' \donttest{
#' ### aliased resource (ie. EDWDBDev or EDWDBProd)
#' edwQuery(sql = "SELECT TOP 10 * FROM ProvidenceEpic.Finance.HospitalAccountBASE",
#'        resource = "EDWDBDev")$data
#'
#'
#' ### Custom resource - any valid ODBC source. If uid & pwd are not specified, then
#' # the Windows (ie. a 'trusted connection' will be used).
#'
#' edwQuery(sql = "SELECT TOP 10 * FROM ProvidenceEpic.Finance.HospitalAccountBASE",
#'        resource = "custom",
#'        custom = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic")$data
#'
#'
#' ### User DSN - need to specify the uid & pwd.
#' # This requires a user or system DSN to be installed on your computer.
#' # The uid and pwd should not be set in the script or in the console for
#' # security reasons. Instead, set them in your .Rprofile file like this:
#
#' options(my_pwd = "mypassword", my_uid = "username")
#'
#' # then in your script, you can load the uid and pwd like this:
#' uid <- getOption("my_uid")
#' pwd <- getOption("my_pwd")

#' edwQuery(sql = "SELECT TOP 10 * FROM [All].core_patient_elements",
#'        resource = "ClinicalAnalytics", DSN = TRUE,
#'        uid=uid, pwd=pwd)$data
#' }



edwQuery <- function (sql="SELECT TOP 10 * FROM Event_Cath", resource = "custom", custom = NULL, DSN=FALSE, uid=NULL, pwd=NULL, ...) {

    start_time <- Sys.time()

    if(DSN) {
        conn <- RODBC::odbcConnect(dsn = resource, uid = uid, pwd = pwd, believeNRows = FALSE)
    } else {
        conn <- RODBC::odbcDriverConnect(connection_string(resource = resource, custom = custom, uid = uid, pwd = pwd, ...))
    }

    sql <- gsub("--.*", "", sql)
    sql <- paste(sql, collapse = " ")

    RODBC::queryResult <- RODBC::sqlQuery(conn, sql, stringsAsFactors = FALSE)
    RODBC::odbcClose(conn)
    elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time,
                                           units = "secs"))
    status_message <- paste0(format(nrow(queryResult), big.mark = ",",
                                    scientific = FALSE, trim = TRUE),
                             " records and ",
                             format(length(queryResult),
                                    big.mark = ",",
                                    scientific = FALSE,
                                    trim = TRUE),
                             " columns were read from ",
                             resource,
                             " in ",
                             round(elapsed_seconds, 2),
                             " seconds.")
    return(list(data = queryResult,
                fields = names(queryResult),
                elapsed_seconds = elapsed_seconds,
                status_message = status_message))
}
