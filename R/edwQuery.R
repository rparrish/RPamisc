#' edw_query
#'
#' queries EDW using windows credentials
#' @param schema - name of the desired schema
#' @param table_name - name of the desired table
#' @param top - integer - use a large number for all records
#' @param fields - single string of field names separated by commas
#' @param where - additional where clauses
#' @param resource - Alias for the database connection
#' @keywords sql
#’ @export
#' @examples
#' sql_query(")
#' 

edwQuery <- function(schema, 
                      table_name, 
                      top=10, 
                      fields="*", 
                      where="1=1", 
                      resource="Phloston"
                      ) {
  
  require(RODBC)
  
  conn <- odbcDriverConnect(connection_string(resource))
  
  # build sql command
  sql.select <- sprintf("SELECT TOP %s %s ",
                        as.integer(top),
                        fields
  )
  sql.from <- sprintf("FROM %s.%s WHERE 1=1 ",
                      schema,
                      table_name)
  
  sql.where <- sprintf("AND %s ", 
                       where)
  
  sql <- paste0(sql.select, sql.from, sql.where)
  
  #result <- sqlTables(conn)
  queryResult <- sqlQuery(conn, 
                          sql,
                          stringsAsFactors=FALSE)
  odbcClose(conn)
  queryResult
  
}

fields <- c("CommunityOrderID, CommunityPatientID, CommunityPatientEncounterID, MedicationDSC")
