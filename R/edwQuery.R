#' edwquery
#'
#' queries EDW using windows credentials
#' @param schema name of the desired schema
#' @param table_name name of the desired table
#' @param max used to return the TOP n records - defaults to 10. Set to FALSE to return all results
#' @param fields single string of field names separated by commas
#' @param where additional WHERE clauses
#' @param order additional ORDER BY clauses
#' @param na.rm remove columns where all values are NA
#' @param resource Alias for the database connection
#' @keywords sql
#' @return Currently, a list is returned with the following elements,
#' \enumerate{
#'  \item \code{data}: an R \code{data.frame} of the desired records and columns.
#'  \item \code{fields}: list of the desired field names.
#'  \item \code{elapsed_seconds}: the duration of the function.
#'  \item \code{status_message}: a boolean value indicating if the operation was apparently successful.
#' }
#' @export
#' @examples
#' edwQuery(schema="Clinical", table="EDEventBASE", max=20)
#'

edwQuery <- function(schema,
                     table_name,
                     max=10,
                     fields="*",
                     where="1=1",
                     order = "",
                     na.rm = TRUE,
                     resource="Phloston"
                     ) {
  start_time <- Sys.time()
  conn <- odbcDriverConnect(connection_string(resource))

  if(max==FALSE) {max=1000000000}

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

  if(order != "") {
    sql.order <- sprintf("ORDER BY %s ",
                       order)
  } else sql.order <- NULL

  sql <- paste0(sql.select, sql.from, sql.where, sql.order)

  #result <- sqlTables(conn)
  queryResult <- sqlQuery(conn,
                          sql,
                          stringsAsFactors=FALSE)
  odbcClose(conn)

  if(na.rm) {queryResult <- queryResult[colSums(is.na(queryResult)) < nrow(queryResult)] }

  elapsed_seconds <- as.numeric(difftime( Sys.time(), start_time, units="secs"))

  status_message <- paste0(format(nrow(queryResult), big.mark = ",", scientific = FALSE, trim = TRUE),
                           " records and ",
                           format(length(queryResult), big.mark = ",", scientific = FALSE, trim = TRUE),
                           " columns were read from ", resource, " in ",
                           round(elapsed_seconds, 2), " seconds.")


  return( list(
    data = queryResult,
    fields = names(queryResult),
    elapsed_seconds = elapsed_seconds,
    status_message = status_message
    )
  )

}


