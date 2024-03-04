library(stringr)
library(httr)
library(logger)
library(proxy)
library(purrr)

if (Sys.getenv("LOG_LEVEL") != "") {
    logger::log_threshold(Sys.getenv("LOG_LEVEL"))
}

# embedding function, task_type:retrieval_query/retrieval_document
prompt_to_embedding <- function(prompt_lines, task_type, task_title=""){
    cfg <- config::get(config = "gemini")
    if(nchar(cfg$api_token) < 1) stop("API token not found.")
    
    url <- paste0(cfg$domain, cfg$embedding_model, "?key=", cfg$api_token)
    headers <- c('Content-Type' = 'application/json')
    model <- paste0("models/", unlist(strsplit(cfg$embedding_model, ":"))[1])
    parts <- lapply(prompt_lines, \(line) {list(text = line)})
    
    body <- list(model = model, 
                 content = list(parts = parts),
                 task_type = task_type,
                 title = task_title)
    resp <- 
        tryCatch(
            {
                httr::POST(
                    url    = url,
                    body   = body,
                    encode = "json",
                    add_headers(.headers=headers)
                )
            }, 
            error = function(e) {
                log_debug(e)
                stop("Cannot send request. Check connection.")
            }
        )
    
    re_embedding <- 
        content(resp) %>% 
        purrr::pluck("embedding", "values") %>% 
        unlist()
    
    return(re_embedding)
}


find_best_template <- function(query){
    cfg <- config::get(config = "template")
    
    tmplts <- read.csv(cfg$embeddings)
    log_info("Read templates embeddings..")
    
    query_emb <- prompt_to_embedding(query, "retrieval_query") 
    
    
    tmplts_emb <- lapply(strsplit(tmplts$Embeddings, ","), \(x) as.numeric(x))
    
    
    similar_score <- 
        tmplts_emb %>% 
        map_vec(~{
            proxy::simil(x = t(query_emb), y = t(.x), method = "cosine") %>% as.vector()
        })
    

    idx <- which.max(similar_score)
    tmplt_file_path <- paste0(cfg$folder, "/", tmplts[idx, 'Title'], ".txt")
    re_tmplt <- 
        list(Title = tmplts[idx, 'Title'],
             Content = readLines(tmplt_file_path))
    
    log_info(str_glue("Find best template:{tmplt_file_path}"))
    
    
    return(re_tmplt)
}


# make prompt and send to api
translate_query_to_sql <- function(query, tmplt) {
    cfg <- config::get(config = "gemini")
    
    if(nchar(cfg$api_token) < 1) stop("API token not found.")
    
    prompt <-
        tmplt$Content %>% 
        map_vec(~ {str_glue(., prompt=query)}) %>%
        as.vector()
    
    url <- paste0(cfg$domain, cfg$text_model, "?key=", cfg$api_token)
    headers <- c('Content-Type' = 'application/json')
    parts <- lapply(prompt, \(l) {list(text = l)})
    
    body <- list(contents = list(parts = parts),
                 generationConfig = list(
                     temperature = cfg$temperature,
                     topK = cfg$topK,
                     topP = cfg$topP,
                     maxOutputTokens = cfg$maxOutputTokens,
                     stopSequences = list(";")
                 ))
    
    log_info("Submit request to Gemini API..")
    
    resp <- 
        tryCatch(
            {
                httr::POST(
                    url    = url,
                    body   = body,
                    encode = "json",
                    add_headers(.headers = headers)
                )
            }, 
            error = function(e) {
                log_debug(e)
                stop("Cannot send request. Check connection.")
            }
        )
    
    resp_text  <- 
        content(resp)[["candidates"]][[1]][["content"]][["parts"]][[1]][["text"]] %>% 
        gsub("```sql|```", "", .)
    
    log_info("Returns {nchar(resp_text)} characters.")

    result <-
        list(sql = resp_text,
             title = tmplt$Title)
    
    log_info("Return sql and template title..")

    return(result)
}
