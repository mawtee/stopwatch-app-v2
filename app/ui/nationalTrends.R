

nationalTrends <- (
  
  # Create National Trends tabPanel and link to Home 
  #========================================================
  tabPanel(
    value = "National Trends:",
    div(div(class="fa fa-line-chart", role = "navigation"), "National Trends"), # TO-FIX: fa-line chart not rendering
    
    
    # Create title/intro box
    #============================================
    useShinyjs(),
    fluidPage(
      
      
      div(
        class='intro-widget-box',
        fluidRow(
          br(),
          br(),
          div(class="page-title", 
            span("National Trends")
          )
        ),
        fluidRow(
          div(class="page-subtitle",
              span("A high-level view of stop and search across England and Wales"),
              br(),
          )
        ),
        fluidRow(
         # div(class="pageview-switch",
              
         # ),
         # add "Select view state"
        
          div(
            class="pageview-togggle",
            column(1,
              shinyWidgets::materialSwitch(
                inputId = "pageview_toggle_nattre",
                status = "primary",
                value=T,
                width='350px'
              )
            )
          ),
          column(6,
            conditionalPanel(
              condition = "input.pageview_toggle_nattre > 0",
              p(HTML("Narrative View (the full experience)"), style = "color: white; font-size: 2.1vw; font-family:   'IBM Plex Mono', sans-serif; margin-left: -30px; margin-top: -2vh; margin-bottom:25vh;")
            ),
            conditionalPanel(
              condition = "input.pageview_toggle_nattre == 0",
              p("Dashboard View (just the data)",  style = "color: white; font-size: 2.1vw; font-family:   'IBM Plex Mono', sans-serif;margin-left: -30px; margin-top: -2vh; margin-bottom:25vh;")
            )
          )
              
            #Toggle.shinyInput("pageview_toggle_nattre")
           # radioSwitchButtons(
            #  inputId = "pageview_toggle_nattre",
            #  label = "",
            #  choices = c("nara", "dash"),
            #  choice_labels = list(
            #    tags$span("Narrative", style = "font-size: 28px; padding: 15px; margin: 60px;",),
            #    tags$span("Dashboard", style = "font-size: 28px; ; padding: 15px; margin: 60px;")
            ##  ),
            #  selected_background = "#e10000"
            #)
          #div(id="pageview-desc-nattre",
          #  p("Our National Trends Narrative highlights the blah and blah of")
         # )
          
        ),

        fluidRow(
          column(1,
            conditionalPanel(
              condition = "input.pageview_toggle_nattre > 0",
              icon("fas fa-chevron-down fa-bounce", "fa-3x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px;")
            ),
            conditionalPanel(
              condition = "input.pageview_toggle_nattre == 0",
              icon("fas fa-chevron-down", "fa-3x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px;") 
            ),
          ),
          column(6,
            p("Scroll down to begin", style="color: white; font-size: 2.1vw; font-family: 'IBM Plex Mono', sans-serif;")
          )
        )
      ),
      
      # tags$i(class = " fa-3x")
      #-------------------------------------------------------------------------
      
      # Scrolly 
      conditionalPanel(
        condition = "input.pageview_toggle_nattre > 0",
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
          fluidRow(
            column(3),
            column(6,align="center",
                   shinyWidgets::sliderTextInput(
                     inputId="year_range", label="Which year(s) would like to visualise?",
                     choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]),
                     force_edges=TRUE
                   )
            ),
            column(3)
            
          ),
          div(id = "end"),
         # fluidRow(
         #   column(3),
         #   column(6,align="center",
         #          shinyWidgets::sliderTextInput(
         #            inputId="year_range", label="Which year(s) would like to visualise?",
         #            choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]),
         #            force_edges=TRUE
          #         )
         #   ),
         #   column(3)
            
         # ),
          br(),
          div(id="begin"),
          
          includeScript('js-assets/intersectionObserver/render_on_view1.js') # AT SAME POINT WORK OUT HOW TO RENDER ONCE bottom of intro page has fully left the viewport
        ),

        scrolly_container(
          "scrcon_nattrend_agg"
          ,
          scrolly_graph(  # RE-DO PLOT IN HIGHCHARTS AND SEE IF CAN REPLICATE SAME EFFECT, THEN DO SO FOR THESE EXAMPLE PLOTS, THEN MOVE ONTO ACTUAL STOP-SEARCH DATA
            div(style="height: 12vh"),
            textOutput("section"),
            div(uiOutput('metric_list'), style = 'font-size: 80%'),
            HTML('<center>'),
            div(
              highchartOutput("plot__pfa_scr_nattrend_agg", height = '95%'),
              style = "height: 85vh; margin: auto; padding-top: 9vh; padding-bottom: 5vh;"),
            HTML('</center>')
            
          )
          ,
          scrolly_sections(
            HTML('<center>'),
            # scrolly_section(id = 0, render_text(0), br(), br(), br(), br(), br()),
            # scrolly_section(id = 1, render_text(1), br(), br(), br(), br(), br()),
            # scrolly_section(id = 2, render_text(2), br(), br(), br(), br(), br()),
            # scrolly_section(id = 3, render_text(3), br(), br(), br(), br(), br()),
            # scrolly_section(id = 4, render_text(4), br(), br(), br(), br(), br()),
            # scrolly_section(id = 5, render_text(5), br(), br(), br(), br(), br()),
            # scrolly_section(id = 6, render_text(6), br(), br(), br(), br(), br()),
            # scrolly_section(id = 7, render_text(7), br(), br(), br(), br(), br()),
            # scrolly_section(id = 8, render_text(8), br(), br(), br(), br(), br()),
            
            #div(scrolly_section(id = 'year', render_text(1)),style = "height: 85vh;margin-top: 45vh;"),
            #div(scrolly_section(id = "buffer1", render_text(0), br())),
            div(scrolly_section(id = 'selfDefinedEthnicityGroup', render_text(2)),style = "height: 85vh;margin-top: 45vh;"),
            div(scrolly_section(id = 'region', render_text(3)),style = "height: 85vh;margin-top: 45vh;"),
            div(scrolly_section(id = 'legislation', render_text(4)),style = "height: 85vh;margin-top: 45vh;"),
            div(scrolly_section(id = 'reasonForSearch', render_text(5)),style = "height: 85vh;margin-top: 45vh;"),
            div(scrolly_section(id = 'outcome', render_text(6)),style = "height: 85vh;margin-top: 45vh;"),
            # 
            # div(scrolly_section(id = 0, render_text(0)),style = "height: 85vh;"),
            # div(scrolly_section(id = 1, render_text(1)),style = "height: 85vh;"),
            # div(scrolly_section(id = 2, render_text(2)),style = "height: 85vh;"),
            # div(scrolly_section(id = 3, render_text(3)),style = "height: 85vh;"),
            # div(scrolly_section(id = 4, render_text(4)),style = "height: 85vh;"),
            # div(scrolly_section(id = 5, render_text(5)),style = "height: 85vh;"),
            # div(scrolly_section(id = 6, render_text(6)),style = "height: 85vh;"),
            # div(scrolly_section(id = 7, render_text(7)),style = "height: 85vh;"),
            # div(scrolly_section(id = 8, render_text(8)),style = "height: 85vh;"),
            
            # div(scrolly_section(id = 0, render_text(0)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 1, render_text(1)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 2, render_text(2)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 3, render_text(3)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 4, render_text(4)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 5, render_text(5)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 6, render_text(6)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 7, render_text(7)),style = paste0("height:", browser_height(), "px;")), 
            # div(scrolly_section(id = 8, render_text(8)),style = paste0("height:", browser_height(), "px;")), 
   


            # add a scrolly_section with nothing in it;
            # this buffer prevents the plot from disappearing while reading last section
            #scrolly_section(id = "buffer2", br()),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            HTML('</center>')
            
          )
          
        ),
        
        br(),
        br(),
        br(),
        
        fluidRow(
          leafletOutput("map")
        )
        
        #leafletOutput("map")
        
      )
    )
  )
)
 


# Number of searches
# By ethnicity
# By legislation
# Reason/Object for search
# Outcome of search

# Select PFA 
# have a map, and click the pfa 
# timeline and counter renders as with above (the base range should be the range selected above)
# drop into maps 
       
      
# add some tabs for number and rate
# get plots looking good, with right colour an rate functionality
# add the text, and some introductory text
# sort out css styles, use scrolly-tell example, see if can replicate exact


#https://www.connorrothschild.com/post/automation-scrollytell


# BIG TEXT NATIONAL TRENDS
# Description text
# Animation on toggle
# if yes, scroll down to scrollytell (with moving icon) beggining with animated map across all years in data
# if no, scroll down (static icon) to dashboard, 
# https://stackoverflow.com/questions/44079923/shiny-resize-other-panels-when-one-of-them-is-set-to-hidden