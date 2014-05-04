#' edwquery
#'
#' queries EDW using windows credentials
#' @param schema - name of the desired schema
#' @param table_name - name of the desired table
#' @param max - used to return the TOP n records - use a large number for all records
#' @param fields - single string of field names separated by commas
#' @param where - additional WHERE clauses
#' @param order - additional ORDER BY clauses
#' @param resource - Alias for the database connection
#' @keywords sql
#’ @export
#' @examples
#' sql_query("")

edwQuery <- function(schema, 
                     table_name, 
                     max=10, 
                     fields="*", 
                     where="1=1",
                     order = NA,
                     resource="Phloston"
                     ) {
  
  require(RODBC)
  
  conn <- odbcDriverConnect(connection_string(resource))
  
  # build sql command
  sql.select <- sprintf("SELECT TOP %s %s ",
                        as.integer(max),
                        fields
  )
  sql.from <- sprintf("FROM %s.%s WHERE 1=1 ",
                      schema,
                      table_name)
  
  sql.where <- sprintf("AND %s ", 
                       where)
  
  if(!is.na(order)) {
    sql.order <- sprintf("ORDER BY %s ", 
                       order)
  }

  sql <- paste0(sql.select, sql.from, sql.where, sql.order)
  
  #result <- sqlTables(conn)
  queryResult <- sqlQuery(conn, 
                          sql,
                          stringsAsFactors=FALSE)
  odbcClose(conn)
  queryResult
  
}


