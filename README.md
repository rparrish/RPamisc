RPamisc
=======

RPamisc is a package with miscellaneous functions that intend to make using R a little easier. The functions range from:  

## SQL functions
 - `sql_query` - generic function to parse and send a sql query from a .sql file or within an R file 
 - `edwTable`, `edwFetch`, & `edwQuery` - wrapper scripts for RODBC functions that return a list of tables, table contents, and a generic sql query
 - apolloQuery
 - `connection_string` - constructs the connection string for RODBC functions using an alias


## Miscelaneous Functions
 - `medd` - calculates the Morphine-Equivalent Daily Dosage of various opioids
 
 
## Analysis


## Graphics
 - `cusumPlot` - generates a basic CUSUM plot. Code adapted from MDRC.
 - `dotplot.errors` - generates a basic dotplot with confidence bars.


## Installation  


This package can be installed using devtools with the following:

    devtools::install_github(build_vignettes = TRUE, "rparrish/RPamisc")
 
## Contributing

- Fork it ( https://github.com/rparrish/RPamisc/fork )
- Create your feature branch (git checkout -b my-new-feature)
- Commit your changes (git commit -am 'Add some feature')
- Push to the branch (git push origin my-new-feature)
- Create a new Pull Request
