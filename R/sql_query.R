#' sql_query
#'
#' queries SQL Server as configured in system DSN
#' @param sql
#' @param dsn
#' @param file (defaults to TRUE)
#' @keywords sql
#’ @export
#' @examples
#' sql_query("select * from table")

sql_query <- function (sql, dsn = "", file = TRUE) {

    if (file) {
        sql <- readLines(sql, ok = TRUE, warn = FALSE)
    }

    sql <- gsub("--.*","", sql)  # remove lines with comments '--'
    sql <- paste(sql,collapse=" ")  # reformat sql to single line

    myconn <- odbcConnect(dsn, uid, pwd)

    mydata <- sqlQuery(myconn, sql)
    close(myconn)

    return(mydata)
}

