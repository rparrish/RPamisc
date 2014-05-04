#' edwFetch
#' 
#' read some or all of a table from the EDW into a data frame
#' @param resource - Alias for the database connection
#' @param schema - name of the desired schema
#' @param tableName - name of the desired table
#' @param catalog - name of the desired database



#' @keywords sql
#’ @export
#' @examples
#' edw_tables("Phloston")
#' 


edwFetch <- function(resource="Phloston",
                     schemaName = NULL,
                     tableName = NULL,
                     max = 10
                      ) {
  
  require(RODBC)
  
  conn <- odbcDriverConnect(connection_string(resource))
  
  
  
  queryResult <- sqlFetch(conn, sqtable = paste(schemaName, tableName, sep="."), max=max)
  
  odbcClose(conn)
  queryResult
  
}
