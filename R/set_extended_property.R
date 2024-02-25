#' set_extended_property
#' 
#' Sets the extended property in SQL server for
#' the specified object (database, schema, table)
#' deferring column properties to a future separate function
#'
#' @param conn 
#' @param catalog 
#' @param schema 
#' @param table 
#' @param name 
#' @param value 
#' 
#' 


set_extended_property <- function(conn, db, schema, table, property, value) {
    
    # check to see if the extended property already exists
    existing <- 
        get_extended_property(conn, db, schema, table, property = property)
    
    sp_name <- "sp_addextendedproperty"
    
    if(nrow(existing) > 0) {
        sp_name <- "sp_updateextendedproperty" 
    } 
    
    sql <- glue("
exec {sp_name} 
     @name = N'{property}' 
    ,@value = N'{value}' 
    ,@level0type = N'schema', @level0name = '{schema}' 
    ,@level1type = N'table',  @level1name = '{table}' 
    ")
    
    sql <- read_sql(sql) 
    rs <- dbExecute(conn, "USE WORKDAY")
    
    rs <- dbGetQuery(conn, sql)
    
}
