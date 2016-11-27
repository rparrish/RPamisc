#' connection_string
#'
#' queries EDW using windows credentials
#' @param resource - Alias for the database connection
#' \describe{
#'   \item{custom}{custom database connection string, as specified in the custom parameter}
#'   \item{EDWDBProd}{EDWDBProd}
#'   \item{Work}{ProvideneEpic_Work}
#'   \item{Trantor}{EDWDBProd}
#'   \item{Phloston}{EDWDBDev}
#'   \item{PHC_CA}{PHC_CA}
#'   \item{PHC_Shared}{PHC_Shared}
#'   \item{PHC_Training}{PHC_Training}
#'   \item{eNICQ}{eNICQ}
#'   \item{ClinicalAnalytics}{\\\\wn2591\\PremierPRD}
#'   \item{Apollo}{\\\\wn1444 Apollo_Spokane}
#' }
#' @param custom a valid SQL connection string. used to connect to other resources (ie. a local SQL Express database)
#' @param uid userid for those resources that require one
#' @param pwd password for those resources that require one. Do not include verbatim passwords in scripts.
#' @return Returns a connection string for odbc
#' @keywords sql


connection_string <- function (resource="custom", custom, uid=getOption("Apollo_uid"), pwd=getOption("Apollo_pwd")) {
    if(resource == "custom" & is.null(custom)) {
        stop("Must specify the custom connection string when resource = 'custom'")
    }
  result <- switch(resource,
                   custom = custom,
                   EDWDBDev = "Driver=SQL Server;Server=EDWDBDev;",
                   EDWDBProd = "Driver=SQL Server;Server=EDWDBProd;",
                   Work = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic_Work;",
                   Trantor = "Driver=SQL Server;Server=EDWDBProd; Database=ProvidenceEpic",
                   Phloston = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic",
                   PHC_CA = "Driver=SQL Server;Server=wn23162;",
                   PHC_Shared = "Driver=SQL Server;Server=wn23162; Database=PHC_Shared",
                   PHC_Training = "Driver=SQL Server;Server=wn23162; Database=PHC_Training",
                   eNICQ = "Driver=SQL Server;Server=wnc1479av\\prodsql2k8a; Database=eNICQ",
                   ClinicalAnalytics = "Driver=SQL Server;Server=wn2591\\PremierPRD; Database=ClinicalAnalytics",
                   Apollo = paste0("Driver=SQL Server;Server=wn57066; Database=Apollo_Spokane; uid=",
                                   uid = getOption("Apollo_uid"), "; pwd=", pwd = getOption("Apollo_pwd"))
                   )
  return(result)
}

