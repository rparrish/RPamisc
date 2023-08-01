#' connection_string
#'
#' Used to query a SQL Server instance using windows credentials
#' @param resource - Alias for the database connection
#' \describe{
#'   \item{custom}{custom database connection string, as specified in the custom parameter}
#'   \item{Trantor}{server_1_name}
#'   \item{Phloston}{server_2_name}
#' } 
#' @param custom a valid SQL connection string. used to connect to other resources (ie. a local DuckDB database) 
#' @param uid userid for those resources that require one 
#' @param pwd password for those resources that require one. Never include verbatim passwords in scripts.
#' 
#' @return Returns a connection string for odbc()
#' @keywords sql
#' @export

connection_string <- function (resource="custom", custom, uid=NULL, pwd=NULL) {
    if(resource == "custom" & is.null(custom)) {
        stop("Must specify the custom connection string when resource = 'custom'")
    }
  result <- switch(resource,
                   custom = custom,
                   Trantor = "Driver=SQL Server;Server=server_1_name;",
                   Phloston = "Driver=SQL Server;Server=server_2_name;"
                   )
  return(result)
}

