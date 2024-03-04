library(config)
library(httr)
library(tidyverse)
cnf <- config::get(config = "gemini")

current_time <- format(Sys.time(), "%H:00:00")

if(!current_time %in% cnf$on_period) stop(paste0(current_time, "禁止提醒时段..."))

prompt <- stringr::str_glue(cnf$prompt) |> as.character()

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

send_msg <- function(msg, webhook){
  
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
  
}

purrr::walk(cnf$webhook, ~ send_msg(msg = msg, webhook = .))



  
