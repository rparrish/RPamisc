#' edwTables
#'
#' queries EDW using windows credentials
#' @param catalog - name of the desired database
#' @param schema - name of the desired schema
#' @param tableName - name of the desired table
#' @param resource - Alias for the database connection
#' @keywords sql
#' @export
#'
#'

edwTables <- function(catalog = NULL,
                       schema = NULL,
                       tableName = NULL,
                       resource="Phloston"
) {


  conn <- RODBC::odbcDriverConnect(connection_string(resource))

  queryResult <- RODBC::sqlTables(conn, catalog=catalog, schema=schema, )

  RODBC::odbcClose(conn)
  queryResult

}

