#' edwFetch
#' 
#' read some or all of a table from the EDW into a data frame
#' @param resource - Alias for the database connection
#' @param schemaName - desired schema
#' @param tableName - desired table
#' @param colnames - desired columns
#' @param max = TOP n rows from the query results
#' @keywords sql
#’ @export
#' @examples
#' edw_tables("Phloston")
#' 


edwFetch <- function(resource="Phloston",
                     schemaName = NULL,
                     tableName = NULL,
                     colnames = FALSE,
                     max = 10
                      ) {
  
  require(RODBC)
  
  conn <- odbcDriverConnect(connection_string(resource))
  
  queryResult <- sqlFetch(conn, 
                          sqtable = paste(schemaName, tableName, sep="."), 
                          colnames = colnames,
                          max=max)
  
  odbcClose(conn)
  queryResult
  
}
