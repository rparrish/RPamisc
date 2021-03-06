% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edwSQL.R
\name{edwSQL}
\alias{edwSQL}
\title{edwSQL}
\usage{
edwSQL(sql = NULL, resource = "custom", custom = NULL, file = TRUE,
  DSN = FALSE, uid = NULL, pwd = NULL, ...)
}
\arguments{
\item{sql}{- name of a .sql file}

\item{resource}{- Alias for the database connection

\describe{
  \item{custom}{allows for a connection to any database server. The connection string must be passed using the custom parameter.}
  \item{EDWDBDev}{EDWDBDev}
  \item{EDWDBProd}{EDWDBProd}
  \item{Trantor}{EDWDBProd}
  \item{Phloston}{EDWDBDev}
  \item{ClinicalAnalytics}{\\\\wn2591\\PremierPRD}
  \item{Apollo}{Apollo_Spokane}
  \item{PHC_CA}{PHC Clinical Analytics Database}
}}

\item{custom}{full connection string when using 'custom' as the resource.}

\item{file}{switch indicating if sql parameter is a .sql file or a command. Default is TRUE (requires .sql file)}

\item{DSN}{if TRUE, use a DSN as specified in `resource` parameter instead of a connection string (ie. for Axis PATS: resource = "TSI_32", DSN = TRUE)}

\item{uid}{username for connections if necessary. Default is NULL, which will then use the user's system credentials.}

\item{pwd}{password for connections that don't use the user's system credentials. Default is NULL.}

\item{...}{additional arguments. not used.}
}
\value{
\item{data}{Query results as a data frame}
\item{fields}{field names}
\item{elapsed_seconds}{elapsed system time to run the query}
\item{status_message}{the number of records & variables in the results}
}
\description{
passes a SQL query from a separate .sql file to the EDW. Commented lines in the .sql file must be preceded by '--'. Blocked comments are not supported.
}
\examples{
\donttest{
### aliased resource (ie. EDWDBDev or EDWDBProd)
mydata <- edwSQL(sql = "SQL/mydata_query.sql",
       resource = "EDWDBDev")$data


### Custom resource - any valid ODBC source. If uid & pwd are not specified, then
# the Windows (ie. a 'trusted connection' will be used).

mydata <- edwSQL(sql = "SQL/mydata_query.sql",
       resource = "custom",
       custom = "Driver=SQL Server;Server=EDWDBDev; Database=ProvidenceEpic")$data


### User DSN - need to specify the uid & pwd.
# This requires a user or system DSN to be installed on your computer.
# The uid and pwd should not be set in the script or in the console for
# security reasons. Instead, set them in your .Rprofile file like this:
options(my_pwd = "mypassword", my_uid = "username")

# then in your script, you can load the uid and pwd like this:
uid <- getOption("my_uid")
pwd <- getOption("my_pwd")
mydata <- edwSQL(sql = "SQL/mydata_query.sql",
       resource = "ClinicalAnalytics", file = FALSE, DSN = TRUE)$data
}
}
\keyword{sql}

