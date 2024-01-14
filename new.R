

library(shiny)
library(scrollrevealR)
library(tidyverse)
library(highcharter)
library(magrittr)

df <- read_csv("data/dfPFA_clean_231009.csv")


dfsum <- df %>%
  group_by(year) %>%
  summarise(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
  ungroup() %>%
  mutate(cumsumOfSearches = cumsum(numberOfSearches))


source("app/funs/countUp_funs.R")

ui <- fluidPage(
  h2("Custom outputs"),
  fluidRow(
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    #includeCSS("counter.css"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    #includeCSS('element.css'),
    br(),
    div(id="begin",
        h3("begin")),
    br(),
    br(),
    br(),
    br(),
    br(),
    #includeCSS(
    #  'counter.css'
    #  
    #),
    div(id = "end"),
    #column(
    #  3, boxxyOutput("subs")
    #),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    includeScript('js-assets/intersectionObserver/render_on_view1.js')
    
    
  )#,
  
  #scroll_reveal(target = c("#subs"), duration = 2000, distance = "100px")
)

server <- function(input, output){
  
  
  
  output$subs <- renderCountUp({
    countUp("Subscriptions", 16719, color = "#ffd166")
  })
  
  output$hc <- renderHighchart({ 
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_yAxis(max = 7000000, min = 0) %>% 
      #hc_xAxis(categories = c(2011:2021)) %>% 
      hc_add_series(index=10, data = list(
        list(sequence = rep(1189882, 11))
      ), dataLabels = list(
        enabled = TRUE,
        #format=round()
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=9, data = list(
        list(sequence = c(rep(NA,1), rep(1017542, 10)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=8, data = list(
        list(sequence = c(rep(NA, 2), rep(904038, 9)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=7, data = list(
        list(sequence = c(rep(NA, 3), rep(541144, 8)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=6, data = list(
        list(sequence = c(rep(NA, 4), rep(383595, 7)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=5, data = list(
        list(sequence = c(rep(NA, 5), rep(304132, 6)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=4, data = list(
        list(sequence = c(rep(NA, 6), rep(282380, 5)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=3, data = list(
        list(sequence = c(rep(NA, 7), rep(385154, 4)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=2, data = list(
        list(sequence = c(rep(NA, 8), rep(579955, 3)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=1, data = list(
        list(sequence = c(rep(NA, 9), rep(715987, 2)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      hc_add_series(index=0, data = list(
        list(sequence = c(rep(NA, 10), rep(530365, 1)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        ))) %>%
      
      hc_colors(rep("#e10000",2)) %>%
      hc_motion(enabled = TRUE, labels = 2011:2021, series = 0:10, autoPlay=T, loop=F) %>%
      hc_plotOptions(column = list(stacking = "normal"))
  }
  )
  
  
  
  
  observeEvent(input$render_hc, {
    
    #  boxxyOutput("subs")
    
    
    #random_numbers_items_batch <- generate_random_numbers_divs(100)
    insertUI(
      selector = "#end",
      where = "afterEnd",
      ui = fluidRow(
        column(
          3, countUpOutput("subs")
        ),
        column(6, highchartOutput("hc"))
      )
      
      #    ui = tags$div(
      #      includeCSS("counter.css"),
      # highchartOutput("hc")
      #   )
    )
  },
  once=T
  )
}
#

#insertUI(
#  selector = "#end",
#  where = "afterEnd",
#  ui = includeCSS("counter.css")
#)



#}


shinyApp(ui, server)

