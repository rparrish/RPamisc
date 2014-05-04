RPamisc
=======

RPamisc is a package with miscellaneous functions that intend to make the R programming process easier. The functions range from:  

 - `sql_query` - generic function to parse and send a sql query from a .sql file or within an R file 
 - `edwTable`, `edwFetch`, & `edwQuery` - wrapper scripts for RODBC functions that return a list of tables, table contents, and a generic sql query
 - `connection_string` - constructs the connection string for RODBC functions using an alias

This package can be installed using devtools with the following:

    devtools::install_github("RPamisc", "rparrish")
 
