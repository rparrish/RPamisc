#' connection_string
#'
#' queries EDW using windows credentials
#' @param resource - Alias for the database connection
#' \describe{
#'   \item{EDWDBProd}{EDWDBProd}
#'   \item{Trantor}{EDWDBProd}
#'   \item{Phloston}{EDWDBDev}
#'   \item{ClinicalAnalytics}{\\\\wn2591\\PremierPRD}
#'   \item{Apollo}{\\\\wn1444 Apollo_Spokane}
#'   \item{Eva}{\\\\wn1444 Testing}
#' }
#' @return Returns a connection string for RODBC
#' @keywords sql



connection_string <- function (resource, database=NULL, uid=getOption("Apollo_uid"), pwd=getOption("Apollo_pwd")) {
  result <- switch(resource,
                   EDWDBDev = "Driver=SQL Server;Server=EDWDBDev;",
                   EDWDBProd = "Driver=SQL Server;Server=EDWDBProd;",
                   Trantor = "Driver=SQL Server;Server=EDWDBProd; Database=ProvidenceEpic",
                   Phloston = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic",
                   ClinicalAnalytics = "Driver=SQL Server;Server=wn2591\\PremierPRD; Database=ClinicalAnalytics",
                   Apollo = paste0("Driver=SQL Server;Server=wn57066; Database=Apollo_Spokane; uid=", uid, "; pwd=", pwd),
                   Eva = paste0("Driver=SQL Server;Server=wn1444; Database=Testing; uid=", uid, "; pwd=", pwd)
                   )
  return(result)
}

