#' edwSQL
#'
#' passes a SQL query to the EDW. Commented lines in the .sql file must be preceded by '--'. Blocked comments are not supported.
#'
#' @param sql - sql query or the path & name of a .sql file
#' @param resource - Alias for the database connection
#'
#' \describe{
#'   \item{custom}{allows for a connection to any database server using the server parameter}
#'   \item{EDWDBDev}{EDWDBDev}
#'   \item{EDWDBProd}{EDWDBProd}
#'   \item{Trantor}{EDWDBProd}
#'   \item{Phloston}{EDWDBDev}
#'   \item{ClinicalAnalytics}{\\\\wn2591\\PremierPRD}
#'   \item{Apollo}{Apollo_Spokane}
#'   \item{Eva}{\\\\wn1444 Testing}
#' }
#'
#' @param server used to specify the database server when resource parameter is not set.
#' @param file switch indicating if sql parameter is a .sql file or a command. Default is TRUE (requires .sql file)
#' @param DSN if TRUE, use a DSN as specified in `resource` parameter instead of a connection string (ie. for Axis PATS: resource = "TSI_32", DSN = TRUE)
#' @param uid username for connections if necessary. Default is NULL, which will then use the user's system credentials.
#' @param pwd password for connections that don't use the user's system credentials. Default is NULL.
#' @return
#' \item{data}{Query results as a data frame}
#' \item{fields}{field names}
#' \item{elapsed_seconds}{elapsed system time to run the query}
#' \item{status_message}{the number of records & variables in the results}
#'
#' @keywords sql
#' @export
#'


edwSQL <- function (sql="SELECT TOP 10 * FROM Event_Cath", resource = "custom", server = NULL, file=TRUE, DSN=FALSE, uid=NULL, pwd=NULL,...) {
    start_time <- Sys.time()

    if(DSN) {
      conn <- odbcConnect(resource = resource, uid = uid, pwd = pwd, believeNRows = FALSE)
       } else {
     conn <- odbcDriverConnect(connection_string(resource = resource, server = server, uid = uid, pwd = pwd, ...))
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
