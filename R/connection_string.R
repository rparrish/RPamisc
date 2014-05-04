#' connection_string
#'
#' queries EDW using windows credentials
#' @param resource - Alias for the database connection
#' @keywords sql
#’ @export
#' @examples
#' connection_string("Trantor")
#' connection_string("Aurora")



connection_string <- function (resource) {
  result <- switch(resource,
                   Trantor = "Driver=SQL Server;Server=EDWDBProd; Database=ProvidenceEpic",
                   Phloston = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic",
                   Aurora = "Driver=SQL Server;Server=EDWDBDev; Database=CatalystStaging",
                   Eva = "wn1444"
                   )
  return(result)
}