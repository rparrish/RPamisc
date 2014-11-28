



# onload

.bg_window_sql <- function(libname, pkgname) {
    readLines( system.file("sql", "bg_window.sql", package="RPamisc"), ok = TRUE, warn = FALSE)
}
