#' get_extended_property
#' 
#' Gets the extended properties in SQL server for
#' the specified object (database, schema, table)
#' deferring column properties to a future separate function
#'
#' @param conn 
#' @param db 
#' @param schema 
#' @param table 
#' @param property 
#' 
#' 


get_extended_property<- 
    function(conn, db, schema, table, property) {
    
    tables <- tbl(conn, in_catalog(db, "sys", "tables"))
    schemas <- tbl(conn, in_catalog(db, "sys", "schemas"))
    all_obj <- tbl(conn, in_catalog(db, "sys", "all_objects")) 
    ext_prop <- tbl(conn, in_catalog(db, "sys", "extended_properties")) 
    
    results_long <- 
        all_obj %>% 
        inner_join(schemas, join_by(schema_id), suffix = c("", ".schemas")) %>% 
        inner_join(tables, join_by(object_id), suffix = c("", ".tables")) %>% #g()
        left_join(ext_prop, join_by("object_id" == "major_id"), suffix = c("", ".prop")) %>% 
        mutate(db = db) %>% 
        select(
            database = db,
            name.schemas,
            name.tables,
            ext_property = name.prop, 
            #type_desc,
            value,
            #everything() 
        ) 
    
    if(!missing(schema) && !is.na(list(schema))) {
        results_long <- 
            filter(results_long, 
                   name.schemas %in% schema)
    } 
    
    if(!missing(table) && !is.na(list(table))) {
        results_long <- 
            filter(results_long, 
                   name.tables %in% table)
    } 
    
    if(!missing(property) && !is.na(list(property))) {
        results_long <- 
            filter(results_long, 
                   ext_property %in% property)
        } 
      
    results_long %>%
        rename(
            "schema" = name.schemas,
            "table" = name.tables) %>%
        collect()
    
}
