library(config)
library(httr)
library(tidyverse)

gemini <- function(prompt){
  url <- paste0(cnf$api, cnf$token)
  headers <- c('Content-Type' = 'application/json')
  
  body <- list(
    contents = list(
      list(
        parts = list(
          list(
            text = prompt
          )
        )
      )
    )
  )
  
  response <- POST(url, body = body, encode = "json", verbose())
  
  msg <- content(response)[["candidates"]][[1]][["content"]][["parts"]][[1]][["text"]]
  
  return(msg)
}



send_msg <- function(msg, webhook){
  
  print(msg)
  
  body <- list(
    msg_type = "text",
    content = list(text = msg)
  )
  
  response <-
    httr::POST(url = webhook,
               body = list(msg_type = "text",
                           content = list(text = msg)),
               encode = "json",
               add_headers("Content-Type" = "application/json"))
  
  if(response$status_code != 200){
    stop(paste0("\n",
                "status_code ", response$status_code))
  }
  
  if(content(response)$code != 0){
    stop(paste0("\n",
                "failed code: ", content(response)$code,
                "\n",
                "failed msg: ", content(response)$msg))
  }
}


# run --------------------------------------------------------------------------
cnf <- config::get(config = "gemini")

current_time <- format(Sys.time(), "%H:00:00")

if(!current_time %in% cnf$crontab) stop(paste0(current_time, "为禁止提醒时段..."))

members <- config::get(config = "members")

member <- sample(members, 1)

member_name <- names(member)
member_id <- member[[1]]

prompt <- stringr::str_glue(paste0(cnf$prompt)) |> as.character()

trials <- 1

while(TRUE){
  gen_text <- gemini(prompt)
  at <- stringr::str_glue("<at user_id=\"{member_id}\"></at>")
  msg <- paste0(gen_text, at)
  
  if(!is.null(gen_text)) {
    purrr::walk(cnf$webhook, ~ send_msg(msg = msg, webhook = .))
    break
  }else(
    trials <- trials + 1
  )
  
  if(trials > cnf$trials) {
    message("尝试次数过多...")
    break
  }
}
