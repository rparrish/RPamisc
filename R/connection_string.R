#' connection_string
#'
#' queries EDW using windows credentials
#' @param resource - Alias for the database connection
#' @keywords sql



connection_string <- function (resource, database=NULL, uid, pwd) {
  result <- switch(resource,
                   Trantor = "Driver=SQL Server;Server=EDWDBProd; Database=ProvidenceEpic",
                   Phloston = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic",
                   ClinicalAnalytics = "Driver=SQL Server;Server=wn2591\\PremierPRD; Database=ClinicalAnalytics",
                   Apollo = "Driver=SQL Server;Server=wn1444; Database=%s; uid=%s; pwd=%s",
                   Eva = sprintf("Driver=SQL Server;Server=wn1444; Database=%s; uid=%s; pwd=%s")
                   )
  return(result)
}

