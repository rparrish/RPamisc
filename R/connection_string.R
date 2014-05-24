#' connection_string
#'
#' queries EDW using windows credentials
#' @param resource - Alias for the database connection
#' @keywords sql
#’ @export



connection_string <- function (resource, database=NULL, uid, pwd) {
  result <- switch(resource,
                   Trantor = "Driver=SQL Server;Server=EDWDBProd; Database=ProvidenceEpic",
                   Phloston = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic",
                   Aurora = "Driver=SQL Server;Server=EDWDBDev; Database=CatalystStaging",
                   Eva = sprintf("Driver=SQL Server;Server=wn1444; Database=%s; uid=%s; pwd=%s",
                                 database,
                                 uid,
                                 pwd)
                   )
  return(result)
}
