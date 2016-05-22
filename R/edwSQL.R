#' edwSQL
#'
#' passes a SQL query from a separate .sql file to the EDW. Commented lines in the .sql file must be preceded by '--'. Blocked comments are not supported.
#'
#' @param sql - name of a .sql file
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
#' @param file switch indicating if sql parameter is a .sql file or a command. Default is TRUE (requires .sql file)
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
#' mydata <- edwSQL(sql = "SQL/mydata_query.sql",
#'        resource = "EDWDBDev")$data
#'
#'
#' ### Custom resource - any valid ODBC source. If uid & pwd are not specified, then
#' # the Windows (ie. a 'trusted connection' will be used).
#'
#' mydata <- edwSQL(sql = "SQL/mydata_query.sql",
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

#' mydata <- edwSQL(sql = "SQL/mydata_query.sql",
#'        resource = "ClinicalAnalytics", file = FALSE, DSN = TRUE)$data
#' }



edwSQL <- function (sql=NULL, resource = "custom", custom = NULL, file=TRUE, DSN=FALSE, uid=NULL, pwd=NULL, ...) {
    requireNamespace("RODBC")

    # sanity tests
    if(missing(sql)) stop("a .sql file must be specified using the sql parameter")


    start_time <- Sys.time()

    if(DSN) {
      conn <- odbcConnect(dsn = resource, uid = uid, pwd = pwd, believeNRows = FALSE)
       } else {
     conn <- odbcDriverConnect(connection_string(resource = resource, custom = custom, uid = uid, pwd = pwd, ...))
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
