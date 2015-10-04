#' edwSQL
#'
#' passes a SQL query to the EDW. Commented lines in the .sql file must be preceded by '--'. Blocked comments are not supported.
#'
#' @param sql - sql query or the path & name of a .sql file
#' @param resource - Alias for the database connection
#'
#' \describe{
#'   \item{Trantor}{EDWDBProd}
#'   \item{Phloston}{EDWDBDev}
#'   \item{ClinicalAnalytics}{\\\\wn2591\\PremierPRD}
#'   \item{Apollo}{\\\\wn1444 Apollo_Spokane}
#'   \item{Eva}{\\\\wn1444 Testing}
#' }
#'
#' @param file switch indicating if sql parameter is a .sql file or a command. Default is TRUE (requires .sql file)
#' @return
#' \item{data}{Query results as a data frame}
#' \item{fields}{field names}
#' \item{elapsed_seconds}{elapsed system time to run the query}
#' \item{status_message}{the number of records & variables in the results}
#'
#' @keywords sql
#' @export
#'


edwSQL <- function (sql="SELECT TOP 10 * FROM Event_Cath", resource = "Apollo", file=TRUE, DSN=FALSE, uid=NULL, pwd=NULL,...) {
    start_time <- Sys.time()

    if(DSN) {
      conn <- odbcConnect(resource, uid=uid, pwd=pwd, believeNRows=FALSE)
       } else {
     conn <- odbcDriverConnect(connection_string(resource, database))
        }
 
    if (file) {
        sql <- readLines(sql, ok = TRUE, warn = FALSE)
    }
    sql <- gsub("--.*", "", sql)
    sql <- paste(sql, collapse = " ")

    queryResult <- sqlQuery(conn, sql, stringsAsFactors = FALSE)
    odbcClose(conn)
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
    return(list(data = queryResult, fields = names(queryResult),
                elapsed_seconds = elapsed_seconds,
                status_message = status_message))
}
