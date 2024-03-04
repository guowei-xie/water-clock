library(shiny)
library(shinyjs)
library(shinycssloaders)
library(bslib)
library(gt)
library(DT)
library(keys)
source("src/cache.R")
source("src/query.R")
source("src/gemini.R")
source("buildUI.R")

server <- function(input, output, session) {
    # focus on prompt upon load
    runjs("document.getElementById('qn').focus()")
    
    # bind hotkeys 
    observeEvent(input$activate, {
        click("btn")
        invalidateLater(3000) # prevent multiple clicks
    })
    
    # highlight code upon tab switched
    observeEvent(input$tabs, {
        delay(150, runjs("hljs.highlightAll();"))
    },
    ignoreInit = TRUE)
    
    # display help guide if pressed
    observeEvent(input$helpguide, {
        showModal(modalDialog(
            title = "艾欧处于测试阶段",
            if (file.exists("static/help.md")) {
                includeMarkdown("static/help.md")
            } else {
                ""
            },
            easyClose = TRUE, 
            footer = modalButton("关闭")
        ))
    })
    
    # read common examples from file 
    if (file.exists("static/examples.txt")) {
        examples <- readLines("static/examples.txt")
    } else {
        examples <- ""
        hide("examples")
    }
    
    # find best template and get translated query
    sql <- eventReactive(input$btn, {
        if(grepl("^sql//", input$qn)){
            # sql// commands query
            list(sql = sub("^sql//", "", input$qn),
                 title = "sql commands")
        }else{
            # natural language query
            template <- find_best_template(input$qn)
            translate_query_to_sql(input$qn, template)
        }
    })
    
    # get result from database
    rdf <- eventReactive(sql(), {
        # reset scrollable container 
        runjs("const container = document.querySelector('.res');
              container.scrollTop = 0;")
        
        res <- send_query_to_database(sql()$sql)
        
        if (is.data.frame(res) && nrow(res) >= 1) {
            cache_this(input$qn, sql()$sql, sql()$title ,1L)
            return(res)
        }
        cache_this(input$qn, sql()$sql, sql()$title, 0L)
        return(NULL)
        
    })
    
    # print sql query 
    output$query <- renderUI({
        if (isTruthy(sql())) {
            q <- sql()$sql
            tags$pre(tags$code(class = "sql", q))
        }
    })
    
    # print nicely formatted table
    output$result <- DT::renderDataTable({
        DT::datatable(
            data = rdf(),
            extension = "Buttons",
            options = list(
                dom='Bfrtip',
                buttons  = c("copy", "csv"),
                columnDefs = list(list(className = "dt-left", targets = "_all")),
                paging = FALSE
            ),
            rownames = FALSE,
            height = "100%"
        )
    })
    
    output$display <- renderUI({
        if (isTruthy(rdf())) {
            DT::dataTableOutput("result", height = "100%")
        } else {
            tags$p(
                "非常抱歉，
               艾欧不明白您的问题。
               能否补充一些说明？
               或者换个问题试试..."
            )
        }
    })
    
    # to rotate common examples 
    output$examples <- renderUI({
        invalidateLater(3000, session)
        txt = paste0("e.g.", sample(examples, 1))
        tags$text(txt)
    })
}

shinyApp(ui = buildUI(), server = server)
