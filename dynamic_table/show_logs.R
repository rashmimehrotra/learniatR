library(shiny)
library(DBI)
library(pool)
library(XML)
library(dplyr)
library(RMySQL)

src <- src_mysql( host = "54.251.104.13",
                      dbname="ejabberd_1609",
                      user = "sanjay",
                      password = rstudioapi::askForPassword("Database password")
)



ui <- fluidPage(
    #dateRangeInput("id", "Enter the date range of sessions:"),
    numericInput("nrows", "How many rows to show?", 10),
    tableOutput("tbl")
    # plotOutput("popPlot")
    
)

server <- function(input, output, session) {
   
    output$tbl <- renderTable({
        #sql <- paste0("SELECT username,bare_peer,txt,id,created_at FROM `archive` order by timestamp desc limit 20")
        #query <- sqlInterpolate(pool, sql, start = input$session_dates[1],end=input$session_dates[2])
        src_pool(poolx) %>% tbl("archive") %>% select(1,2,4,6,10) %>% arrange(desc(timestamp)) %>% head(input$nrows)
     #  y<-lapply(x$txt,xmlTreeParse)
        # for ( i in 1:25){
        #     y[[i]][1]$doc$children$Message ->x[i]
        # }
        on.exit(dbDisconnect(poolx))
    })

    # output$popPlot <- renderPlot({
    #     query <- paste0("SELECT count(month(starts_on)) as TOT FROM  class_sessions LIMIT",
    #                     as.integer(input$nrows), ";")
    #     df <- dbGetQuery(pool, query)
    #     total_sessions <- df$TOT
    #     #names(pop) <- df$Name
    #     barplot(total_sessions)
    # })
}

shinyApp(ui, server)
