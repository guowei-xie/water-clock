library(RMySQL)
library(DBI)
library(logger)

send_query_to_database <- function(sql) {   
    
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
    
    log_info("Successful connected to database")
    
    res <- 
        tryCatch(
            {   
                r <- DBI::dbGetQuery(conn, sql)
                r
            },
            error = function(e) {
                print(e)
                NA_integer_
            }
        )

    #print(res)
    
    DBI::dbDisconnect(conn)
    
    if (is.data.frame(res)) {
        log_info("Result has {nrow(res)} rows")
        return(res)
    } else {
        log_info("Not a dataframe")
        print(res)
        return(NULL)
    }
}
