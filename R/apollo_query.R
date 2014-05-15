#' apollo_query
#'
#' queries Apollo Server as configured in system DSN
#' @param sql
#' @param uid userID
#' @param pwd password
#' @param file (defaults to TRUE)
#' @keywords sql
#’ @export
#' @examples
#' sql_query("select * from table")

apollo_query <- function (sql, uid, pwd, file = TRUE) {

    if (file) {
        sql <- readLines(sql, ok = TRUE, warn = FALSE)
    }

    sql <- gsub("--.*","", sql)  # remove lines with comments '--'
    sql <- paste(sql,collapse=" ")  # reformat sql to single line

    myconn <- odbcConnect(, uid, pwd)

    mydata <- sqlQuery(myconn, sql)
    close(myconn)

    return(mydata)
}

