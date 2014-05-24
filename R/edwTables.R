#' edwTables
#'
#' queries EDW using windows credentials
#' @param catalog - name of the desired database
#' @param schema - name of the desired schema
#' @param tableName - name of the desired table
#' @param resource - Alias for the database connection
#' @keywords sql
#’ @export


edwTables <- function(catalog = NULL,
                       schema = NULL,
                       tableName = NULL,
                       tableType = NULL,
                       resource="Phloston"
) {

  require(RODBC)

  conn <- odbcDriverConnect(connection_string(resource))



  queryResult <- sqlTables(conn, catalog=catalog, schema=schema, )

  odbcClose(conn)
  queryResult

}

