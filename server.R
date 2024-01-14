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
    plot__pfa_scr_nattrend_agg(df_pfa, input$year_range, input$scrcon_nattrend_agg)
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


