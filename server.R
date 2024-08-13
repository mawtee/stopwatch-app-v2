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
  
  output$map__pfa_scr_nattrend_agg <- renderHighchart({
    plot__pfa_map(df_pfa, bounds_pfa, input$year_range, input$pfa_select)
  })
  
  output$map__pfa_pie <- renderHighchart({
    plot__pfa_map_pie(df_pfa, bounds_pfa, input$year_range, input$pfa_select)
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
    plot__pfa_scr_nattrend_agg(df_pfa, input$year_range, input$year_range_scr, input$scrcon_nattrend_agg, input$ethnic_group_scr, input$region_group_scr, input$legislation_group_scr, input$reason_group_scr, input$outcome_group_scr)
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
  
  
  
###############################################################################
  # 
  # output$hc4 <- renderHighchart({ 
  #   
  #   highchart() %>% 
  #     hc_chart(type = "column", inverted=T) %>% 
  #     hc_xAxis(categories=0,
  #              gridLineColor= 'transparent',
  #              labels=list(enabled=F),
  #              lineColor= 'transparent', lineWidth= 0, tickLength=0) %>%
  #     hc_yAxis(max = 7000000, min = 0, gridLineColor= 'transparent', lineColor= 'transparent', lineWidth= 0) %>% 
  #     #hc_xAxis(categories = c(2011:2021)) %>% 
  #     hc_add_series(index=10, id="anno1",  data = list(
  #       list(sequence = rep(1189882, 11))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       #format=round()
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=9, id="anno2",  data = list(
  #       list(sequence = c(rep(NA,1), rep(1017542, 10)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=8, id="anno3", data = list(
  #       list(sequence = c(rep(NA, 2), rep(904038, 9)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=7, data = list(
  #       list(sequence = c(rep(NA, 3), rep(541144, 8)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=6,  data = list(
  #       list(sequence = c(rep(NA, 4), rep(383595, 7)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=5, data = list(
  #       list(sequence = c(rep(NA, 5), rep(304132, 6)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=4, data = list(
  #       list(sequence = c(rep(NA, 6), rep(282380, 5)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=3, data = list(
  #       list(sequence = c(rep(NA, 7), rep(385154, 4)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=2, data = list(
  #       list(sequence = c(rep(NA, 8), rep(579955, 3)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=1, data = list(
  #       list(sequence = c(rep(NA, 9), rep(715987, 2)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_add_series(index=0, data = list(
  #       list(sequence = c(rep(NA, 10), rep(530365, 1)))
  #     ), dataLabels = list(
  #       enabled = TRUE,
  #       style = list(
  #         textShadow = F,
  #         textOutline = F,
  #         fontWeight = 'bold',
  #         opacity = 1
  #       )), pointWidth=42) %>%
  #     hc_annotations(
  #       list(
  #         labelOptions = list(
  #           shape = "connector",
  #           align = "right",
  #           justify = FALSE,
  #           crop = TRUE,
  #           borderColor= '#e10000',
  #           borderRadius=30,
  #           backgroundColor='yellow',
  #           style = list(
  #             fontSize = "0.8em",
  #             textOutline = "1px white",
  #             backgroundColor='yellow'
  #           )
  #         ),
  #         labels = list(
  #           list(point = list(x = 0, y = 500000, xAxis = 0, yAxis = 0, linkedTo="anno1"), text = "value1"),
  #           list(point = list(x = 0, y = 1500000, xAxis = 0, yAxis = 0, linkedTo="anno2"), text = "value2" ),
  #           list(point = list(x = 0, y = 6000000, xAxis = 0, yAxis = 0, linkedTo="anno3"), text = "value3")
  #         )
  #       )
  #     ) %>%
  #   
  #   
  #     
  #     hc_colors(rep("#e10000",2)) %>%
  #     hc_motion(enabled = TRUE, labels = 2011:2021, series = 0:10, autoPlay=T, loop=F) %>%
  #     hc_plotOptions(column = list(stacking = "normal"))
  # }
  # )
  
 # 1. Intro page
  ##############################################################################
  
  
  # Plot outputs
  #=============================================================================
  
  # Line chart
  output$plotOutput__natS1_line <- renderHighchart({
    isolate({ #'* Isolates the initial render from the subsequent proxy updates, enabling slider functionality for both series *
      plotFun__natS1_line(df_pfa, input$year_range) 
    })
  })
  
  

  
  # Timeline chart
  browser_width <- reactive({
    shinybrowser::get_width()
  })
  browser_height <- reactive({
    shinybrowser::get_height()
  })
  output$plotOutput__natS1_timeline <- renderHighchart({ 
    plotFun__natS1_timeline(df_pfa, input$year_range, browser_width(), browser_height())
  }
  )
  
  
  output$plotOutput__natS1_2011timeline <- renderHighchart({ 
  df <- data.frame(
    date=as.Date(c("01-04-2011", "01-06-2011", "01-11-2011", "01-02-2012"),format = "%d-%m-%Y"),
    event=c("X was searched", "X policy was introduced", "Z was stopped and falsey arrested", "Data showed 2011 was highest on record"),
    type=c("#E10000", "#F6B7B7","#E10000", "#F6B7B7"),
    description=c(paste0("description blah", 1:4))
  )  
  df %>% 
    hchart("timeline", hcaes(x = date,
                             label = paste0("<b>", event, "</b>"),
                             color = type,
                             name = paste0(date),
                             description=description
    ),
    dataLabels = list(allowOverlap = FALSE),
    linkedTo = "color",
    showInLegend = F) %>% 
    hc_yAxis(visible = FALSE) %>% 
    hc_xAxis(dateTimeLabelFormats = list(month = "%d-%m-%Y"), 
             type = "datetime"   
             
    )
  }
  )
  
  
  output$plotOutput__natS1_2012timeline <- renderHighchart({ 
    df <- data.frame(
      date=as.Date(c("01-04-2012", "01-06-2012", "01-11-2012", "01-02-2013"),format = "%d-%m-%Y"),
      event=c("X was searched", "X policy was introduced", "Z was stopped and falsey arrested", "Data showed 2011 was highest on record"),
      type=c("#E10000", "#F6B7B7","#E10000", "#F6B7B7"),
      description=c(paste0("description blah", 1:4))
    )  
    df %>% 
      hchart("timeline", hcaes(x = date,
                               label = paste0("<b>", event, "</b>"),
                               color = type,
                               name = paste0(date),
                               description=description
      ),
      dataLabels = list(allowOverlap = FALSE),
      linkedTo = "color",
      showInLegend = F) %>% 
      hc_yAxis(visible = FALSE) %>% 
      hc_xAxis(dateTimeLabelFormats = list(month = "%d-%m-%Y"), 
               type = "datetime"   
               
      )
  }
  )
  
  
  
  output$plotOutput__natS2_item <- renderHighchart({
    #isolate({ #'* Isolates the initial render from the subsequent proxy updates, enabling slider functionality for both series *
      plotFun__natS2_item(df_pfa, input$year_range) 
    #})
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # countUp outputs
  #=============================================================================
  
  # Number of searches
  #--------------------

  # Reactive based on current year selection
  reactive__natS1_nSearches <- reactive({
    year_range_int <- which(levels(df_pfa$year) %in% input$year_range)
    df_stat <- df_pfa %>%
      ungroup() %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      slice(1) %>%
      select(numberOfSearches)
    countTo <- df_stat$numberOfSearches[1]
  })

  # Define countUp reactive values based on previous values
  reactiveVal__natS1_nSearches_countFromTo <-  reactiveValues(countFrom=0, countTo=0)
  observeEvent(
    reactive__natS1_nSearches(),{
      reactiveVal__natS1_nSearches_countFromTo$countFrom <- reactiveVal__natS1_nSearches_countFromTo$countTo;
      reactiveVal__natS1_nSearches_countFromTo$countTo <- reactive__natS1_nSearches()
    }
  )
  reactive__natS1_nSearches_countTo <- reactive({req(reactive__natS1_nSearches());  reactive__natS1_nSearches(); reactiveVal__natS1_nSearches_countFromTo$countTo})
  reactive__natS1_nSearches_countFrom <- reactive({req(reactive__natS1_nSearches()); reactive__natS1_nSearches(); reactiveVal__natS1_nSearches_countFromTo$countFrom})
  output$natS1_nSearches_countTo <- renderPrint({sprintf("countTo:%d", natS1_nSearches_countTo())})
  output$natS1_nSearches_countFrom <- renderPrint({sprintf("countFrom:%d", natS1_nSearches_countFrom())})

  output$natS1_nSearches_countUp_text <- renderUI({
    tagList(
      h3(paste0("were recorded in England and Wales between ", input$year_range[1]," and ",input$year_range[2]), style="text-align:left;float: left; font-family: 'IBM Plex Mono', sans-serif; font-size: 2.5vh; margin-left: 2.75vw; margin-top: -1vh; color: #333333;")
    )
  })

  
  
  # output$natS2_text <- renderUI({
  #   tagList(
  #     typedjs::typed(
  #       c("<span style ='text-align:left;float: left;  margin-left: 2.75vw; margin-top: 4vh; font-family: Public Sans, sans-serif; font-size: 6.5vh; color: #cacaca;'>For every 100 searches</span"),
  #       contentType = "html", typeSpeed = 20, showCursor=F
  #     ),
  #     br(),
  #     #  h3(paste0("conducted between ", input$year_range[1]," and ",input$year_range[2]), style="text-align:left;float: left; font-family: 'IBM Plex Mono', sans-serif; font-size: 2.5vh; margin-left: 4.8vw; margin-top: 1.8vh; color: #333333;")
  #     
  #     typedjs::typed(
  #       c("<span style ='text-align:left;float: left; font-size: 2.5vh; margin-left: 4.8vw; margin-top: 1.8vh;'>blah blah blah</span", "<span style ='text-align:left;float: left; font-family: IBM Plex Mono, sans-serif; font-size: 2.5vh; margin-left: 4.8vw; margin-top: 1.8vh; color: #333333;'>conducted between i and j</span"),
  #       contentType = "html", typeSpeed = 20, showCursor=F, startDelay=4000, backdelay=4000
  #     )
  #     # typedjs::typed(
  #     #   c("<span style ='text-align:left;float: left;  margin-left: 2.75vw; margin-top: 4vh; font-family: Public Sans, sans-serif; font-size: 6.5vh; color: #cacaca;'>For every 100 searches</span"),
  #     #   contentType = "html", typeSpeed = 25, showCursor=F, backDelay=2000
  #     # ),
  #     # typedjs::typed(
  #     #   c("<span style ='text-align:left;float: left; font-family: IBM Plex Mono, sans-serif; font-size: 2.5vh; margin-left: 4.8vw; margin-top: 1.8vh; color: #333333;'>conducted between i and j</span"),
  #     #   contentType = "html", typeSpeed = 25, showCursor=F, startDelay=2000
  #     # ),
  #     # 
  #     
  #    #  
  #    #  h3("For every 100 searches", style="text-align:left;float: left; font-family: 'Public Sans', sans-serif; font-size: 6.5vh; margin-left: 2.75vw; margin-top: 4vh; color: #cacaca;"),
  #    # # h3("100 searches ", style="text-align:left;float: left; font-family: 'Public Sans', sans-serif; font-size: 6.5vh; margin-left: 2.75vw; margin-top: 4vh; color: #cacaca;"),
  #    #  h3(paste0("conducted between ", input$year_range[1]," and ",input$year_range[2]), style="text-align:left;float: left; font-family: 'IBM Plex Mono', sans-serif; font-size: 2.5vh; margin-left: 4.8vw; margin-top: 1.8vh; color: #333333;")
  #   )
  #   
  #    
  # })
  

  typedjs::typed(
    c("<span style ='font-size: 6vh; font-family: IBM Plex Mono, sans-serif;  color: #333333;'>Stop and search is not working...</span",''),
    contentType = "html", typeSpeed = 20, showCursor=F, backDelay=2000, backSpeed = 25
  )
  
  
  
  reactiveVal__stage <- reactiveVal(0) 
  
  observeEvent(input$year_range_confirm, {
    newstage <- reactiveVal__stage() + 1     
    reactiveVal__stage(newstage)  
  },
  once=T)
  
  

  
  output$typd <- renderTyped(
    type(reactiveVal__stage())
  )





#   # Arrest rate reactive
#   reactive__nattre_intro_arrestRate <- reactive({
#     year_range_int <- which(levels(df_pfa$year) %in% input$year_range)
#     df_stat <- df_pfa %>%
#       ungroup() %>%
#       mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
#       mutate(arrest = case_when(outcome == "Arrest"~1, T~0)) %>%
#       mutate(numberOfArrests = sum(arrest)) %>%
#       summarise(arrestRate = (numberArrest/numberOfSearches)*100) %>%
#       slice(1)
#     countTo <- df_stat$arrestRate[1]
#   })
#   
#   
#   
#   # output$countUp_pfa_0 <- renderCountup({
#   #   #print()
#   #   #browser()
#   #   #if(input$pfa_select %in% unique(df_pfa$pfaName)){
#   #     countUp_pfa(reactiveVal__countFromTo_pfa$countTo, reactiveVal__countFromTo_pfa$countFrom)
#   #   #}
#   # }
#   # )
#   
#   output$countUp_pfa_text <- renderUI({
#     tagList(
#       h3(paste0("were recorded by ",input$pfa_select, " Police between ", input$year_range[1]," and ",input$year_range[2]), style="text-align:left;float: left; font-family: 'IBM Plex Mono', sans-serif; font-size: 2.5vh; margin-left: 2.75vw; margin-top: -1vh; color: #333333;")
#     )
#   })
#   
#   
#   
  
  

  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  observeEvent(input$render__natS1_ui, {
    
    #  boxxyOutput("subs")
    delay(1000,
    # insert ui, removeUI
    #https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
    #random_numbers_items_batch <- generate_random_numbers_divs(100)
    insertUI(
      selector = "#natS1-contents",
      where = "beforeEnd",
      ui = 
        tags$div(
          id='natS1-contents-phase1',
          div(
            #div()
          fluidRow(
            column(7, 
              div(countUp_pfa(reactiveVal__natS1_nSearches_countFromTo$countTo, reactiveVal__natS1_nSearches_countFromTo$countFrom, 3.35),style='margin-left:.5vw; font-size:6vh'),
              h2("stop-searches", style="font-size: 4.5vh; color: #e10000; font-weight:bold; margin-top: -2.5vh; margin-left: 2.75vw; font-family: 'Public Sans', sans-serif;"),
              uiOutput('natS1_nSearches_countUp_text')
            ),
            #column(5, div(id='natS1-contents-phase2', style='height:65vh'))
          ),
          div(
          fluidRow(
            column(6,
              highchartOutput('plotOutput__natS1_line', height='65vh')
            ) # , style='height:90vh'
          )#,
          #style='height:3vh'
          )
                   
                   
            # TODO PROXYYYYYY
            #https://github.com/jbkunst/highcharter/blob/main/dev/sandbox/proxy-shiny.R
            #div(combineWidgetsOutput('column__pfa_scr_nattrend_agg', height='100%'), style='height: 37vh; width: 45vw; margin-left: -1.5vw;')
        # fluidRow(
        #   column(3, countUpOutput("subs")), column(8, p("stop-searches were recorded in England and Wales between 2011/12 and 2021/22", style="font-size: .5vh; font-family: 'Public Sans', sans-serif; margin-top:7vh"))
        #   )
        )
      
      #    ui = tags$div(
      #      includeCSS("counter.css"),
      # highchartOutput("hc")
      #   )
        )
    )
    )
    
    # TODO Might wanna put this into a function
    year_range_int <- which(levels(df_pfa$year) %in% input$year_range)
    df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
    df_pfa_plot <- df_pfa %>%
      group_by(year) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
      ungroup() %>%
      distinct(year, .keep_all=T) %>%
      select(year, numberOfSearches)
    
    df_pfa_high_plot <- df_pfa_plot
    df_pfa_high_plot$index <- paste(1:nrow(df_pfa_high_plot))
    df_pfa_high_plot$flag <- ifelse(df_pfa_high_plot$numberOfSearches == max(df_pfa_high_plot$numberOfSearches),1,0)
    df_pfa_high_plot$numberOfSearches <- ifelse(df_pfa_high_plot$flag==1, df_pfa_high_plot$numberOfSearches, 0)
    #browser()
    #browser()
    df_pfa_high_low_plot <- df_pfa_plot
    df_pfa_high_low_plot$flag <- ifelse(df_pfa_high_low_plot$numberOfSearches == max(df_pfa_high_low_plot$numberOfSearches),1,
                                        ifelse(df_pfa_high_low_plot$numberOfSearches == min(df_pfa_high_low_plot$numberOfSearches),1,0))
    df_pfa_high_low_plot$numberOfSearches <- ifelse(df_pfa_high_low_plot$flag==1, df_pfa_high_low_plot$numberOfSearches, 0)
    
    max <- max(df_pfa_plot$numberOfSearches)
    min <- min(df_pfa_plot$numberOfSearches)
    median <- median(df_pfa_plot$numberOfSearches)
    df_pfa_plot$text[1] <- ""
    anno <- df_pfa_plot %>%
      select()
    
    delay(1000,#6500,
          highchartProxy('plotOutput__natS1_line') %>%
            hcpxy_add_series(
              type='column', id='ts', name='In-year', data=df_pfa_high_plot$numberOfSearches, color="#e10000", yAxis = 0, zIndex=1#, dataLabels=list(enabled=T)
            ) %>%
            hcpxy_update(
              annotations=
                list(
                  labels = list(
                    list(point = list(x = 0, y = max, xAxis = 0, yAxis = 0), text = '<div style="color:#333333; font-size: 2vh;" >2011/12 was the highest year on record')
                    #list(point = list(x = 7, y = min, xAxis = 0, yAxis = 1), text = '<div style="color:#333333; font-size: 2vh;" >2018/19 was the lowest year on record')
                  )
                )
            )
          # hc
          
    )    
    delay(1000,#11000,
          highchartProxy('plotOutput__natS1_line') %>%
            hcpxy_update_series(
              id = "ts",
              data=df_pfa_high_low_plot$numberOfSearches
            ) %>%
            # hcpxy_update_point(
            #   id='ts', id_point=ind, y=min, shift = TRUE
            # ) %>%
            hcpxy_update(
              annotations=
                list(
                  labels = list(
                    list(point = list(x = 0, y = max, xAxis = 0, yAxis = 0), text = '<div style="color:#333333; font-size: 2vh;" >2011/12 was the highest year on record'),
                    list(point = list(x = 6, y = min, xAxis = 0, yAxis = 0), text = '<div style="color:#333333; font-size: 2vh;" >2017/18 was the lowest year on record')
                  )
                )
            )
          # hcpxy_remove_point(
          #   id='ts',i=0
          # )
          #id_point=df_pfa_low_plot$index[1],x=df_pfa_low_plot$year[1], y=df_pfa_low_plot$numberOfSearches[1]  
    )
    
    delay(1000,#13000,
          highchartProxy('plotOutput__natS1_line') %>%
          hcpxy_update(
            yAxis=list(

              plotLines= list(
                list(
                  color= '#333333',
                  label = list(text = paste0("Median = ",  median(df_pfa_plot$numberOfSearches)),
                               align='right', y=-10, style=list(fontSize='2vh', fontStyle='italic', textOutline= '3px contrast', color='#333333')),
                  width= 3,
                  value= median(df_pfa_plot$numberOfSearches),
                  dashStyle='LongDash',
                  zIndex= 5
                )
              )
            )
          )
    )
    
    
    
    delay(1000,#15500,
          highchartProxy('plotOutput__natS1_line') %>%
            hcpxy_update_series(
              id = "ts",
              data=df_pfa_plot$numberOfSearches
            ) %>%
            #TODO Add median line before rending all series - because axis_multiples is problem try setting line to axis 1 and and column to axis 0
            # hcpxy_update(
            #   yAxis=list(
            #     
            #     plotLines= list(
            #       list(
            #         color= '#333333',
            #         label = list(text = "Median"),
            #         width= 2,
            #         value= median,
            #         yAxis=1,
            #         zIndex= 5
            #       )
            #     )
            #   )
              #   
              # ) %>%
            hcpxy_update(
              annotations= list(
                labels = list(
                )
              )
            )
    )
    
    
    # TODO possibly add area spline https://stackoverflow.com/questions/36610669/highcharts-areaspline-highlight-a-column-on-hover-effect
    delay(1000,#18500,
          # insert ui, removeUI
          #https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
          #random_numbers_items_batch <- generate_random_numbers_divs(100)
          # removeUI(
          #   selector = "#natS1-contents-phase1"
          # )
          insertUI(
            selector = "#natS1-contents-phase2",
            where = "beforeEnd",
            ui = 
              tags$div(
                id='natS1-contents-phase2',
                div(
                  div(style='height: 4.5vh'),
                  #div(style='margin-left: 4.5vw;',
                  fluidRow(
                    column(11, 
                      typedjs::typed(c("<span style ='font-size: 2.5vh; font-family: IBM Plex Mono, sans-serif;  color: #333333;'>For more detailed insight on each year, take a scroll through our stop and search timeline below</span"),
                                             contentType = "html", typeSpeed = 20, showCursor=F),
                      div(style='height:2vh'),
                      shinyjs::hidden(
                        div(id='glideshit',
                      glide(
                        screen(
                          navset_card_tab(
                            title='2011/12',
                            # TODO move title alignment to right (!proven v difficult so far)
                            #id='navv',
                            nav_panel("Timeline", align='left',
                                      highchartOutput('plotOutput__natS1_2011timeline', height='45vh')
                            ),
                            nav_menu(
                              title = 'Sources', align='left',
                              nav_panel(' Event 1',
                                        tags$iframe(style="height:45vh; width:100%; margin-top:2vh; scrolling=yes",src="2011.html")
                              ),
                              nav_panel(' Event 2',
                                        tags$iframe(style="height:45vh; width:100%; margin-top:2vh; scrolling=yes",src="2011.html")
                              ),
                              nav_panel(' Event 3',
                                        tags$iframe(style="height:45vh; width:100%; margin-top:2vh; scrolling=yes",src="2011.html")
                              )
                              
                            )
                          ),
                          next_label = paste("Next yr", shiny::icon("chevron-right", lib = "glyphicon")),
                          previous_label = paste(shiny::icon("chevron-left", lib = "glyphicon"), "Back yr")
                        ),
                        screen(
                          p("Second screen."),
                          next_label = paste("Next yr", shiny::icon("chevron-right", lib = "glyphicon")),
                          previous_label = paste(shiny::icon("chevron-left", lib = "glyphicon"), "Back yr")
                        )
                      )
                        )
                      )
                      
                    )
                  )
                  #)
      
                )
              )
          )
    )
    
    delay(1000,#28000,
      insertUI(
      selector = "#natS1-contents-phase3-text",
      where = "beforeEnd",
      ui = 
        tags$div(
          id='natS1-contents-phase3-text',
      div(style='height:3vh'),
      fluidRow(
        column(12,
               typedOutput('typd')
      # typedjs::typed(c("<span style ='font-size: 2.5vh; font-family: IBM Plex Mono, sans-serif;  color: #333333;'>When you're ready, confirm which years (or year) you want to visualise...</span"),
      #                contentType = "html", typeSpeed = 20)
        )
      )
      )
      )
    )
                     
      
      
          
# 
#     delay(60000000,
#           insertUI(
#             selector = "#natS1-contents",
#             where = "beforeEnd",
#             ui = 
#               tags$div(id='natS1-contents-phase2',
#                        fluidPage(
#                          div(
#                            fluidRow(
#                              column(2),
#                              column(4, highchartOutput('plotOutput__natS1_timeline')),
#                              column(2)
#                           
#                            ),
#                          
#                          )
#                        )
#               )
#           )
#     )
    
  },
  once=T
  )
  
  observeEvent(input$render__natS1_ui, {
    delay(1000,#30500,
          show("year_range", anim=T, animType='fade', time=1)
          
          )
    delay(1000,#30500,
          show("year_range_confirm", anim=T, animType='fade', time=1)
          
    )
    
  })
  #21000
  
  observeEvent(input$render__natS1_ui, {
    delay(1000, # 24000
          show("glideshit", anim=T, animType='fade', time=1)) 
  })
  
  observeEvent(input$year_range, {
    year_range_int <- which(levels(df_pfa$year) %in% input$year_range)
    #browser()
    if (length(year_range_int)==1) {
      df_pfa <- df_pfa[df_pfa$year == levels(df_pfa$year)[year_range_int[1]],]
    }
    else {
      df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
   }
    df_pfa_plot <- df_pfa %>%
      group_by(year) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
      ungroup() %>%
      distinct(year, .keep_all=T) %>%
      mutate(numberOfSearches_cumsum = cumsum(numberOfSearches)) %>%
      select(year, numberOfSearches, numberOfSearches_cumsum)
    highchartProxy('plotOutput__natS1_line') %>%
      hcpxy_update_series(
        id = "ts",
        data=df_pfa_plot$numberOfSearches
      ) %>%
      hcpxy_update_series(
        id = "line",
        data=df_pfa_plot$numberOfSearches_cumsum
      ) %>%
      hcpxy_update(
        yAxis=list(
          
          plotLines= list(
            list(
              color= '#333333',
              label = list(text = paste0("Median = ",  median(df_pfa_plot$numberOfSearches)),
                           align='right', y=-10, style=list(fontSize='2vh', fontStyle='italic', textOutline= '3px contrast', color='#333333')),
              width= 3,
              value= median(df_pfa_plot$numberOfSearches),
              dashStyle='LongDash',
              zIndex= 5
            )
          )
        )
      )
  }
  )
  
  
  
  # Section Two
  #=============================================================================
  
  
  observeEvent(input$render__natS2_ui, {
  
    year_range_int <- which(levels(df_pfa$year) %in% input$year_range)
    df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
    
    df_pfa_plot <- df_pfa %>%
      group_by(outcome) %>%
      summarise(numberOfSearches_N = sum(numberOfSearches, na.rm=T)) %>%
      pivot_wider(names_from='outcome', values_from='numberOfSearches_N') %>%
      mutate(arrestRate = round(Arrest/(Arrest+`No Arrest`)*100,0)) %>%
      mutate(nonArrestRate = round(100-arrestRate,0))
    
    #browser()
    delay(
      1000,
      insertUI(
        selector = "#natS2-contents",
        where = "beforeEnd",
        ui = tags$div(
          id='natS2-contents-phase1',
          div(style='height: 20vh'),
          fluidRow(
            column(2),
            column(8,
              typedjs::typed(
                c("<span style ='font-size: 6vh; font-family: IBM Plex Mono, sans-serif;  color: #333333;'>Stop and search is not working...</span",''),
                contentType = "html", typeSpeed = 20, showCursor=F, backDelay=2000, backSpeed = 25
              )
            ),
            column(2)
          )
        )
      )
    )
    delay(
      6000,
      removeUI(
        selector = '#natS2-contents-phase1'
      )
    )
    delay(
      7000,
      insertUI(
        selector = '#natS2-content-phase2-textA',
        where = 'beforeEnd',
        ui = tags$div(
          id='natS2-content-phase2-textA',
          div(style='height: 5vh'),
          typedjs::typed(
            c("<span style ='text-align:left;float: left;  margin-left: 2.75vw; margin-top: 4vh; font-family: Public Sans, sans-serif; font-size: 6.5vh; color: #cacaca;'>For every 100 searches</span"),
            contentType = "html", typeSpeed = 20, showCursor=F
          )
        )
      )
    )
    delay(
      7000,
      insertUI(
        selector = '#natS2-content-phase2-item',
        where = 'beforeEnd',
        ui = tags$div(
          id='natS2-content-phase2-item',
          div(style='height: 5vh'),
          highchartOutput('plotOutput__natS2_item', height='35vh', width='40vw')
        )
      )
    )
    
    delay(
      7800,
      insertUI(
        selector = '#natS2-content-phase2-textB',
        where = 'beforeEnd',
        ui = tags$div(
          id='natS2-content-phase2-textB',
         # style='text-align:left;float: left; margin-left: 4.8vw; margin-top: 1.8vh;',
          typedjs::typed(
            c("<span style ='text-align:left;float: left; margin-left: 4.8vw; margin-top: 1.5vh; font-family: IBM Plex Mono, sans-serif; font-size: 2.5vh; color: #333333;'>conducted between i and j</span"),
            contentType = "html", typeSpeed = 25, showCursor=F
          )
        )
      )
    )
    
    
    
    delay(
      9000,
      
      highchartProxy('plotOutput__natS2_item') %>%
        hcpxy_remove_point(
          id='outcomes',
          i=1,
          redraw=F
        ) %>%
        
      hcpxy_add_point(
        id='outcomes',
        point= list(
          name ='Arrests',
          y=df_pfa_plot$arrestRate[1],
          color='#e10000',
          marker=list(
            symbol='url(data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCA2NDAgNTEyIj48IS0tIUZvbnQgQXdlc29tZSBGcmVlIDYuNi4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlL2ZyZWUgQ29weXJpZ2h0IDIwMjQgRm9udGljb25zLCBJbmMuLS0+PHBhdGggZmlsbD0iI2UxMDAwMCIgZD0iTTI0MCAzMmEzMiAzMiAwIDEgMSA2NCAwIDMyIDMyIDAgMSAxIC02NCAwek0xOTIgNDhhMzIgMzIgMCAxIDEgMCA2NCAzMiAzMiAwIDEgMSAwLTY0em0tMzIgODBjMTcuNyAwIDMyIDE0LjMgMzIgMzJsOCAwYzEzLjMgMCAyNCAxMC43IDI0IDI0bDAgMTZjMCAxLjctLjIgMy40LS41IDUuMUMyODAuMyAyMjkuNiAzMjAgMjg2LjIgMzIwIDM1MmMwIDg4LjQtNzEuNiAxNjAtMTYwIDE2MFMwIDQ0MC40IDAgMzUyYzAtNjUuOCAzOS43LTEyMi40IDk2LjUtMTQ2LjljLS40LTEuNi0uNS0zLjMtLjUtNS4xbDAtMTZjMC0xMy4zIDEwLjctMjQgMjQtMjRsOCAwYzAtMTcuNyAxNC4zLTMyIDMyLTMyem0wIDMyMGE5NiA5NiAwIDEgMCAwLTE5MiA5NiA5NiAwIDEgMCAwIDE5MnptMTkyLTk2YzAtMjUuOS01LjEtNTAuNS0xNC40LTczLjFjMTYuOS0zMi45IDQ0LjgtNTkuMSA3OC45LTczLjljLS40LTEuNi0uNS0zLjMtLjUtNS4xbDAtMTZjMC0xMy4zIDEwLjctMjQgMjQtMjRsOCAwYzAtMTcuNyAxNC4zLTMyIDMyLTMyczMyIDE0LjMgMzIgMzJsOCAwYzEzLjMgMCAyNCAxMC43IDI0IDI0bDAgMTZjMCAxLjctLjIgMy40LS41IDUuMUM2MDAuMyAyMjkuNiA2NDAgMjg2LjIgNjQwIDM1MmMwIDg4LjQtNzEuNiAxNjAtMTYwIDE2MGMtNjIgMC0xMTUuOC0zNS4zLTE0Mi40LTg2LjljOS4zLTIyLjUgMTQuNC00Ny4yIDE0LjQtNzMuMXptMjI0IDBhOTYgOTYgMCAxIDAgLTE5MiAwIDk2IDk2IDAgMSAwIDE5MiAwek0zNjggMGEzMiAzMiAwIDEgMSAwIDY0IDMyIDMyIDAgMSAxIDAtNjR6bTgwIDQ4YTMyIDMyIDAgMSAxIDAgNjQgMzIgMzIgMCAxIDEgMC02NHoiLz48L3N2Zz4=)'
          )
        ),
        shift=F,
        animation=list(
          enabled=T, duration=0
        )
       ) %>%
        hcpxy_remove_point(
          id='outcomes',
          i=0,
          redraw=F
        ) %>%

        hcpxy_add_point(
          id='outcomes',
          point= list(
            name ='No Arrests',
            y=df_pfa_plot$nonArrestRate[1],
            color='#333333',
            marker=list(
              symbol='url(data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAzMjAgNTEyIj48IS0tIUZvbnQgQXdlc29tZSBGcmVlIDYuNi4wIGJ5IEBmb250YXdlc29tZSAtIGh0dHBzOi8vZm9udGF3ZXNvbWUuY29tIExpY2Vuc2UgLSBodHRwczovL2ZvbnRhd2Vzb21lLmNvbS9saWNlbnNlL2ZyZWUgQ29weXJpZ2h0IDIwMjQgRm9udGljb25zLCBJbmMuLS0+PHBhdGggZmlsbD0iIzMzMzMzMyIgZD0iTTExMiA0OGE0OCA0OCAwIDEgMSA5NiAwIDQ4IDQ4IDAgMSAxIC05NiAwem00MCAzMDRsMCAxMjhjMCAxNy43LTE0LjMgMzItMzIgMzJzLTMyLTE0LjMtMzItMzJsMC0yMjMuMUw1OS40IDMwNC41Yy05LjEgMTUuMS0yOC44IDIwLTQzLjkgMTAuOXMtMjAtMjguOC0xMC45LTQzLjlsNTguMy05N2MxNy40LTI4LjkgNDguNi00Ni42IDgyLjMtNDYuNmwyOS43IDBjMzMuNyAwIDY0LjkgMTcuNyA4Mi4zIDQ2LjZsNTguMyA5N2M5LjEgMTUuMSA0LjIgMzQuOC0xMC45IDQzLjlzLTM0LjggNC4yLTQzLjktMTAuOUwyMzIgMjU2LjkgMjMyIDQ4MGMwIDE3LjctMTQuMyAzMi0zMiAzMnMtMzItMTQuMy0zMi0zMmwwLTEyOC0xNiAweiIvPjwvc3ZnPg==)'
              
              #symbol='circle'
            )
          ),
          shift=F,
          animation=list(
            enabled=T, duration=3000
          )
        ) 
      # TODO update series rather than remove
      # TODO try just adding individual series
      
      
    )
    
    
 
    
    

  },
  once=T
  )

  

  # countUP PFA
  #############################################################################
  
  reactive__numberOfSearches_pfa <- reactive({
    
    year_range_int <- which(levels(df_pfa$year) %in% input$year_range)
    df_pfa <- df_pfa %>%
         group_by(pfaName) %>%
         mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
         #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
         ungroup() %>% 
         distinct(pfaName, .keep_all=T) %>%
         select(pfaName, numberOfSearches)
    countTo <- df_pfa$numberOfSearches[df_pfa$pfaName==input$pfa_select]
    
  })
  
  reactiveVal__countFromTo_pfa <-  reactiveValues(countFrom=0, countTo=0)
 
  observeEvent( 
    reactive__numberOfSearches_pfa(),{
      reactiveVal__countFromTo_pfa$countFrom <- reactiveVal__countFromTo_pfa$countTo; 
      #print(reactiveVal__countFromTo_pfa$countFrom)
      reactiveVal__countFromTo_pfa$countTo <- reactive__numberOfSearches_pfa()
      #print(reactiveVal__countFromTo_pfa$countTo)
    }
  )
 
  reactive__countTo_pfa <- reactive({req(reactive__numberOfSearches_pfa());  reactive__numberOfSearches_pfa(); reactiveVal__countFromTo_pfa$countTo})
  reactive__countFrom_pfa <- reactive({req(reactive__numberOfSearches_pfa()); reactive__numberOfSearches_pfa(); reactiveVal__countFromTo_pfa$countFrom})
  
  
  output$countTo_pfa <- renderPrint({sprintf("countTo:%d", reactive__countTo_pfa())})
  output$countFrom_pfa <- renderPrint({sprintf("countFrom:%d", reactive__countFrom_pfa())})
  
  
  # output$countUp_pfa_0 <- renderCountup({
  #   #print()
  #   #browser()
  #   #if(input$pfa_select %in% unique(df_pfa$pfaName)){
  #     countUp_pfa(reactiveVal__countFromTo_pfa$countTo, reactiveVal__countFromTo_pfa$countFrom)
  #   #}
  # }
  # )
  
  output$countUp_pfa_text <- renderUI({
    tagList(
      h3(paste0("were recorded by ",input$pfa_select, " Police between ", input$year_range[1]," and ",input$year_range[2]), style="text-align:left;float: left; font-family: 'IBM Plex Mono', sans-serif; font-size: 2.5vh; margin-left: 2.75vw; margin-top: -1vh; color: #333333;")
    )
  })
  
  output$column__pfa_scr_nattrend_agg <- renderCombineWidgets({
    plot__pfa_quintiles(df_pfa, input$year_range, input$pfa_select)
  }
  )
  
  observeEvent(
    input$pfa_select,once=T, ignoreNULL=T, {
      if(input$pfa_select %in% unique(df_pfa$pfaName)){
        insertUI(
          selector = "#countUp-pfa-ui",
          where = "afterEnd",
          ui = tagList(
            fluidRow(style='margin-top: 1.5vh',
              div(countUp_pfa(reactiveVal__countFromTo_pfa$countTo, reactiveVal__countFromTo_pfa$countFrom),style='margin-left:.5vw'),
              h2("stop-searches", style="font-size: 5.5vh; color: #e10000; font-weight:bold; margin-top: -2.5vh; margin-left: 2.75vw; font-family: 'Public Sans', sans-serif;"),
              uiOutput('countUp_pfa_text'),
              div(style='height:9vh'),
              div(combineWidgetsOutput('column__pfa_scr_nattrend_agg', height='100%'), style='height: 37vh; width: 45vw; margin-left: -1.5vw;')
              # h2("stop-searches", style="font-size: 7vh; color: #e10000; font-weight:bold; margin-top: -4vh; margin-left: 3vw; font-family: 'Public Sans', sans-serif;"),
              # h3(paste0("were recorded in ",input$pfa_select, " between ", input$year_range[1]," and ",input$year_range[2]), style="text-align:left;float: left; font-family: 'IBM Plex Mono', sans-serif; font-size: 3vh; margin-left: 3vw; margin-top: -1vh;")
            )
          )
        )
      }
    }
  )
    
  observeEvent(reactiveVal__countFromTo_pfa$countTo, {
    countupProxy("countUp-pfa") %>% 
      countup_update(reactiveVal__countFromTo_pfa$countTo)
  })
  

  
  
################################################################################  
  


  
  
  #h3("stop-searches were recorded in x between t and t1")
  # output$countUp_pfa <- renderCountup({
  #   countUp_pfa(reactiveVal__countFromTo_pfa$countTo, reactiveVal__countFromTo_pfa$countFrom)
  # })
  # 
  # 
  # observeEvent(input$pfa_select!, {
  #   browser()
  #   if(input$pfa_select %in% unique(df_pfa$pfaName)){
  #     browser()
  #     insertUI(
  #       selector = "#add_ui_here",
  #       where = "afterEnd",
  #       ui = countUp_pfa(reactiveVal__countFromTo_pfa$countTo, reactiveVal__countFromTo_pfa$countFrom)
  #       
  #     )
  #   }
  # },
  # once=T, ignoreNULL=T
  # )
  # 
  # # observeEvent()
  # 
  # 
  # 
  # observeEvent(reactiveVal__countFromTo_pfa$countTo, {
  #   if(input$pfa_select %in% unique(df_pfa$pfaName)){
  #     countupProxy("countUp_pfa") %>% 
  #       countup_update(reactiveVal__countFromTo_pfa$countTo)
  #   }
  # })
  # 
  
  
  output$introPlot <- renderPlotly({
    introPlot
  })
  output$scr <- renderScrollytell({
    scrollytell()
  })
  # renderText(paste0("Section: ", input$scr))
  # observe({
  #   cat("section:", input$scr, "\n")
  # })
  
  
  output$metric_list <- renderUI({
    radioGroupButtons(
      inputId = "metric", label = "",
      choices = c('Number of stop-searches', 'Rate of stop-search per 1,000 people'),
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
      choices=c(paste0(year_range_list()[1],"-",year_range_list()[length(year_range_list())]), year_range_list()[1:length(year_range_list())]), selected=paste0(year_range_list()[1],"-",year_range_list()[length(year_range_list())])
    )
  }
  )
  
  # observeEvent(c(input$year_range,year_range_list()), {
  #   shinyWidgets::updateSliderTextInput(
  #     session, inputId="year_range_sidebar", 
  #     choices=year_range_list()[1:length(year_range_list())], selected=year_range_list()[1])
  # }
  # )
  # 
  # 
  
  
  # 
  # shinyWidgets::sliderTextInput(
  #   inputId="year_range", label="Which year(s) would like to visualise?",
  #   choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]),
  #   force_edges=TRUE
  # )

  output$sidebar_ui <- renderUI({

    myvalue <- input$scrcon_nattrend_agg
    ethnic_group <- ifelse(length(input$ethnic_group_scr)==length(unique(df_pfa$selfDefinedEthnicityGroup)), "All", ifelse(length(input$ethnic_group_scr)==0, "None", input$ethnic_group_scr))
    #region_group <-  ifelse(length(input$region_group_scr)==length(unique(df_pfa$region)), "All", ifelse(length(input$region_group_scr)==0, "None", input$region_group_scr))
    legislation_group <- ifelse(length(input$legislation_group_scr)==length(unique(df_pfa$legislation)), "All", ifelse(length(input$legislation_group_scr)==0, "None" ,input$legislation_group_scr))
    reason_group <- ifelse(length(input$reason_group_scr)==length(unique(df_pfa$reasonForSearch)), "All", ifelse(length(input$reason_group_scr)==0, "None" ,input$reason_group_scr))
    outcome_group <- ifelse(length(input$outcome_group_scr)==length(unique(df_pfa$outcome)), "All", ifelse(length(input$outcome_group_scr)==0, "None", input$outcome_group_scr))
    
    if(is.null(myvalue)) {
    }
    
    else if(myvalue == "selfDefinedEthnicityGroup") {
      div(class="parent",
          div(class="divy2", HTML(paste0(HTML("<b>Ethnic Group</b><br>Selected: "), HTML(ethnic_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Legislation</b><br>Selected: "), HTML(legislation_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Reason for Search</b><br>Selected: "), HTML(reason_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Outcome of Search</b><br>Selected:"), HTML(outcome_group)))),
          
          style="width:95%; height:15vh;")
    }
    else if(myvalue == "legislation") {
      div(class="parent",
          div(class="divy", HTML(paste0(HTML("<b>Ethnic Group</b><br>Selected: "), HTML(ethnic_group)))),
          div(class="divy2", HTML(paste0(HTML("<b>Legislation</b><br>Selected: "), HTML(legislation_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Reason for Search</b><br>Selected: "), HTML(reason_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Outcome of Search</b><br>Selected:"), HTML(outcome_group)))),
          
          style="width:95%; height:15vh;")
    }
    else if(myvalue == "reasonForSearch") {
      div(class="parent",
          div(class="divy", HTML(paste0(HTML("<b>Ethnic Group</b><br>Selected: "), HTML(ethnic_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Legislation</b><br>Selected: "), HTML(legislation_group)))),
          div(class="divy2", HTML(paste0(HTML("<b>Reason for Search</b><br>Selected: "), HTML(reason_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Outcome of Search</b><br>Selected:"), HTML(outcome_group)))),
          
          style="width:95%; height:15vh;")
    }
    else if(myvalue == "outcome") {
      div(class="parent",
          div(class="divy", HTML(paste0(HTML("<b>Ethnic Group</b><br>Selected: "), HTML(ethnic_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Legislation</b><br>Selected: "), HTML(legislation_group)))),
          div(class="divy", HTML(paste0(HTML("<b>Reason for Search</b><br>Selected: "), HTML(reason_group)))),
          div(class="divy2", HTML(paste0(HTML("<b>Outcome of Search</b><br>Selected:"), HTML(outcome_group)))),
          
          style="width:95%; height:15vh;")
    }
    
    else {
      
    }

  })
  
  
  
  output$dashboard_chart <- renderHighchart({
    plot__dashboard_chart(df_pfa, input$year_range_dash, input$metric_dash, input$ethnic_group_dash) 
  })



}


