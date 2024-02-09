#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  output$hc_more <- renderHighchart({
    plot__pfa_tsline_nat_hc(df_pfa, input$year_range, input$legislation_select)
  })
  
  output$map <- renderHighchart({
    plot__pfa_map(df_pfa, bounds_pfa, input$year_range)
  })
  
  output$hc_rep2 <- renderHighchart({
    plot__pfa_tsline_nat_hc(df_pfa, input$year_range, input$legislation_select)
  })
  
  output$plot2 <- renderImage({
    outfile <- tempfile(fileext='.gif')
    plot__pfa_tsline_nat_anim(df_pfa, input$year_range, input$legislation_select)
    list(src = "outfile.gif",
         contentType = 'image/gif',
          #width = 400,
          height = 300
         # alt = "This is alternate text"
    )
    }, deleteFile = T)
  
  
  output$hc_more2 <- renderHighchart({
    hchart(diamonds$carat)
  })
  
  output$hc_rep1 <- renderHighchart({
    hchart(diamonds$carat)
  })
  
  
  output$plot__pfa_scr_nattrend_agg <- renderHighchart ({
    plot__pfa_scr_nattrend_agg(df_pfa, input$year_range, input$year_range_scr, input$scrcon_nattrend_agg)
  })
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    col <- input$scrcon_nattrend_agg
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = col, border = 'white')
  })
  
  output$scrcon_nattrend_agg <- renderScrollytell({scrollytell()})
  #output$section <- renderText(paste0("Section: ", input$scrcon_nattrend_agg))
  
  observe({cat("section:", input$scrcon_nattrend_agg, "\n")})
  
  
  
  
  
  output$subs <- renderCountUp({
    countUp("Number of stop and searches in the England & Wales", 1671794, color = "#fff")
  })
  
  output$hc4 <- renderHighchart({ 
    
    highchart() %>% 
      hc_chart(type = "column", inverted=T) %>% 
      hc_xAxis(categories=0,
               gridLineColor= 'transparent',
               labels=list(enabled=F),
               lineColor= 'transparent', lineWidth= 0, tickLength=0) %>%
      hc_yAxis(max = 7000000, min = 0, gridLineColor= 'transparent', lineColor= 'transparent', lineWidth= 0) %>% 
      #hc_xAxis(categories = c(2011:2021)) %>% 
      hc_add_series(index=10, id="anno1",  data = list(
        list(sequence = rep(1189882, 11))
      ), dataLabels = list(
        enabled = TRUE,
        #format=round()
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=9, id="anno2",  data = list(
        list(sequence = c(rep(NA,1), rep(1017542, 10)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=8, id="anno3", data = list(
        list(sequence = c(rep(NA, 2), rep(904038, 9)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=7, data = list(
        list(sequence = c(rep(NA, 3), rep(541144, 8)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=6,  data = list(
        list(sequence = c(rep(NA, 4), rep(383595, 7)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=5, data = list(
        list(sequence = c(rep(NA, 5), rep(304132, 6)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=4, data = list(
        list(sequence = c(rep(NA, 6), rep(282380, 5)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=3, data = list(
        list(sequence = c(rep(NA, 7), rep(385154, 4)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=2, data = list(
        list(sequence = c(rep(NA, 8), rep(579955, 3)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=1, data = list(
        list(sequence = c(rep(NA, 9), rep(715987, 2)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_add_series(index=0, data = list(
        list(sequence = c(rep(NA, 10), rep(530365, 1)))
      ), dataLabels = list(
        enabled = TRUE,
        style = list(
          textShadow = F,
          textOutline = F,
          fontWeight = 'bold',
          opacity = 1
        )), pointWidth=42) %>%
      hc_annotations(
        list(
          labelOptions = list(
            shape = "connector",
            align = "right",
            justify = FALSE,
            crop = TRUE,
            borderColor= '#e10000',
            borderRadius=30,
            backgroundColor='yellow',
            style = list(
              fontSize = "0.8em",
              textOutline = "1px white",
              backgroundColor='yellow'
            )
          ),
          labels = list(
            list(point = list(x = 0, y = 500000, xAxis = 0, yAxis = 0, linkedTo="anno1"), text = "value1"),
            list(point = list(x = 0, y = 1500000, xAxis = 0, yAxis = 0, linkedTo="anno2"), text = "value2" ),
            list(point = list(x = 0, y = 6000000, xAxis = 0, yAxis = 0, linkedTo="anno3"), text = "value3")
          )
        )
      ) %>%
    
    
      
      hc_colors(rep("#e10000",2)) %>%
      hc_motion(enabled = TRUE, labels = 2011:2021, series = 0:10, autoPlay=T, loop=F) %>%
      hc_plotOptions(column = list(stacking = "normal"))
  }
  )
  output$hc <- renderHighchart({ 
    plot__nattrend_timeline(df_pfa, input$year_range, browser_width(), browser_height())
  }
  )
  
  observeEvent(input$render_hc, {
    
    #  boxxyOutput("subs")
    
    
    #random_numbers_items_batch <- generate_random_numbers_divs(100)
    insertUI(
      selector = "#end",
      where = "afterEnd",
      ui = 
        fluidPage(
        fluidRow(
          column(3, countUpOutput("subs"))
          ),
      fluidRow(
        column(1),
          column(10, highchartOutput("hc")),
        column(10)
      )
    )
      
      #    ui = tags$div(
      #      includeCSS("counter.css"),
      # highchartOutput("hc")
      #   )
    )
  },
  once=T
  )
  

  
  
  browser_width <- reactive({
    shinybrowser::get_width()
  })
  
  browser_height <- reactive({
    shinybrowser::get_height()
  })
  
  
  
  
  output$introPlot <- renderPlotly({
    introPlot
  })
  output$scr <- renderScrollytell({
    scrollytell()
  })
  renderText(paste0("Section: ", input$scr))
  observe({
    cat("section:", input$scr, "\n")
  })
  
  
  output$metric_list <- renderUI({
    radioGroupButtons(
      inputId = "metric", label = "",
      choices = c('Number of stop-searches', 'Rate of stop-search per 1,000 people', 'Ratio of stop-searches to recorded crimes'),
      selected = 'Number of stop-searches',
      size = 'sm',
      justified = TRUE,
      status = "default"
    )
  })
  
  
  
  year_range_list <- reactive({
    levels(df_pfa$year)[which(levels(df_pfa$year) %in% input$year_range)[1]:which(levels(df_pfa$year) %in% input$year_range)[2]]
  })
  
  
  observeEvent(c(input$year_range,year_range_list()), {
    shinyWidgets::updateSliderTextInput(
      session, inputId="year_range_scr", 
      choices=year_range_list()[1:length(year_range_list())], selected=year_range_list()[1])
  }
  )
  # 
  # observeEvent(c(input$year_range,year_range_list()), {
  #   shinyWidgets::updateSliderTextInput(
  #     session, inputId="year_range_scr3", 
  #     choices=year_range_list()[1:length(year_range_list())], selected=year_range_list()[1])
  # }
  # )
  # 
  # observeEvent(c(input$year_range,year_range_list()), {
  #   shinyWidgets::updateSliderTextInput(
  #     session, inputId="year_range_scr4", 
  #     choices=year_range_list()[1:length(year_range_list())], selected=year_range_list()[1])
  # }
  # )
  # 
  # observeEvent(c(input$year_range,year_range_list()), {
  #   shinyWidgets::updateSliderTextInput(
  #     session, inputId="year_range_scr5", 
  #     choices=year_range_list()[1:length(year_range_list())], selected=year_range_list()[1])
  # }
  # )
  # 
  # observeEvent(c(input$year_range,year_range_list()), {
  #   shinyWidgets::updateSliderTextInput(
  #     session, inputId="year_range_scr6", 
  #     choices=year_range_list()[1:length(year_range_list())], selected=year_range_list()[1])
  # }
  # )
    
  #   
  # output$year_range_scr <- renderUI({
  #   shinyWidgets::sliderTextInput(
  #     inputId="year_range_scr", label="Press play to animate", 
  #     choices=year_range_list()[1:length(year_range_list())], selected=year_range_list()[1],
  #     animate=T)
    #animate =
  
      #animate =
       # animationOptions(loop = TRUE))


}


