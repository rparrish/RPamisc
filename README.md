RPamisc
=======

RPamisc is a package with miscellaneous functions that intend to make using R a little easier. The functions range from:  

## SQL functions
 - `sql_query` - generic function to parse and send a sql query from a .sql file or within an R file 
 - `edwTable`, `edwFetch`, & `edwQuery` - wrapper scripts for RODBC functions that return a list of tables, table contents, and a generic sql query
 - apolloQuery
 - `connection_string` - constructs the connection string for RODBC functions using an alias


## Analysis


## Graphics


## Installation  


This package can be installed using devtools with the following:

    devtools::install_github("RPamisc", "rparrish")
 
