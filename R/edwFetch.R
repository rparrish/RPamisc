#' edwFetch
#'
#' read some or all of a table from the EDW into a data frame
#' @param resource - Alias for the database connection
#' @param schema - desired schema
#' @param table - desired table
#' @param max = TOP n rows from the query results
#' @keywords sql
#' @export
#'


edwFetch <- function(resource="Phloston",
                     schema = NULL,
                     table = NULL,
                     max = 10
                      ) {


  conn <- RODBC::odbcDriverConnect(connection_string(resource))

  queryResult <- RODBC::sqlFetch(conn,
                          sqtable = paste(schema, table, sep="."),
                          max=max)

  RODBC::odbcClose(conn)
  queryResult

}
