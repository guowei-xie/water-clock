library(RMySQL)
library(DBI)
library(ids)

cache_this <- function(sql, query, tmplt_title, result) {
    db_cfg <- config::get(config = "database")
    
    conn <- DBI::dbConnect(
        drv = RMySQL::MySQL(),
        host = db_cfg$host,
        user = db_cfg$user,
        password = db_cfg$pwd,
        port = db_cfg$port,
        db = db_cfg$database
    )
    
    re <- DBI::dbSendQuery(conn, "SET NAMES UTF8")
    DBI::dbClearResult(re)
    
    cch_cfg <- config::get(config = "cache")
    
    rec <- data.frame(
        timestamp = Sys.time(),
        sign   = ids::random_id(n = 1, bytes = 3),
        query = query,
        template = tmplt_title,
        sql  = sql,
        result = result
    )
    
    if (!cch_cfg$tblname %in% dbListTables(conn, cch_cfg$dbname)) {
        dbWriteTable(conn,  cch_cfg$tblname, rec)
    } else {
        dbWriteTable(conn,  cch_cfg$tblname, rec, append = TRUE)
    }
    
    log_debug(sql)
    log_info("Cached prompt and query")
    dbDisconnect(conn)
    return(query)
}