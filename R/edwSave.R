#' edwSave
#'
#' Saves a data frame to the specified resource
#' #'
#' @param df - data.frame
#' @param tablename - name of table on database
#' @param resource - Alias for the database connection
#'
#' \describe{
#'   \item{custom}{allows for a connection to any database server. The connection string must be passed using the custom parameter.}
#'   \item{Work}{ProvidenceEpic_Work on EDWDBDev}
#' }
#'
#' @param safer logical. If true, create a non-existing table but only allow appends to an existing table. 
#' If false, allow sqlSave to attempt to delete all the rows of an existing table, or to drop it
#' @param DSN if TRUE, use a DSN as specified in `resource` parameter instead of a connection string (ie. for Axis PATS: resource = "TSI_32", DSN = TRUE)
#' @param uid username for connections if necessary. Default is NULL, which will then use the user's system credentials.
#' @param pwd password for connections that don't use the user's system credentials. Default is NULL.
#' @param message display the time required to complete?
#' @param ... arguments to be passed to RODBC::odbcDriverConnect
#' @return
#' \item{status_message}{}
#'
#' @keywords sql
#' @export
#'
#'


edwSave <- function (df=mtcars, tablename, resource = "Work",  safer = TRUE, DSN=FALSE, uid=NULL, pwd=NULL, message = FALSE, ...) {

    # sanity tests
    if(missing(df)) stop("df must be a data.frame")

    if(missing(tablename)) stop("must specify a tablename")
  
    start_time <- Sys.time()

    if(DSN) {
      conn <- RODBC::odbcConnect(dsn = resource, uid = uid, pwd = pwd, believeNRows = FALSE)
       } else {
     conn <- RODBC::odbcDriverConnect(RPamisc:::connection_string(resource = resource, custom = NULL, uid = uid, pwd = pwd))
        }

    queryResult <- RODBC::sqlSave(conn, df, tablename = tablename, rownames = FALSE, append = FALSE, safer = safer, ...)
    RODBC::odbcClose(conn)
    elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time,
                                           units = "secs"))
    status_message <- paste0(deparse(substitute(df)),
                             " saved to ",
                             tablename, 
                             " on ",
                             resource,
                             " in ",
                             round(elapsed_seconds, 2),
                             " seconds.")
    if(message) {cat(status_message)}
    
    #return(list(
    #            elapsed_seconds = elapsed_seconds,
    #            status_message = status_message))
    
}
