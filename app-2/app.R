library(shiny)
library(lubridate)
library(rsconnect)
library(dplyr)
library(ggplot2)
#setwd("~/Dropbox/R-wd/app-1")

rm(list=ls())
load("subset_variables")
load("learniat_variables_copy")

# quarters<-ques_log_exp$starts_on %>%
#   quarter(with_year = T) %>% 
#   sort() %>% 
#   unique()

class_day<-date(ques_log_exp$starts_on) %>%
  sort() %>% 
  unique()

d$stud_session_time %>% 
  group_by(student_id,class_session_id) %>% 
  summarise_at(vars(student_gi_num,student_gi_den,student_pi),sum) %>% 
  top_n(10,student_pi) %>% 
  print(n=30)


ui <- fluidPage(
  sliderInput(inputId = "range",
    label = h4("Position the sliders to choose a date range:"),
    value = class_day[c(1,100)], min = class_day[1], max = class_day[244]),
  plotOutput("hist")
)


server <- function(input, output) {
 output$hist <- renderPlot({
   dt1<-input$range[1]
   dt2<-input$range[2]
   range_interval<-interval(dt1,dt2)

   ques_log_exp %>%
  filter(int_overlaps(interval(starts_on,starts_on),range_interval),
         !topic_id %in% c(264),
         !class_id %in% c("10th grade Physics section A 3")) %>%
    ggplot(aes(class_id,fill=topic_id)) +
    geom_bar(position="stack") +ylab("Count of Answers")

 })
}

shinyApp(ui = ui, server = server)