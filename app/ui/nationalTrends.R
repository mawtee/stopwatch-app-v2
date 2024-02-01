

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
              icon("fa-solid fa-basketball fa-bounce", "fa-3x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px;")
            ),
            conditionalPanel(
              condition = "input.pageview_toggle_nattre == 0",
              icon("fa-solid fa-basketball", "fa-3x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px;") 
            ),
          ),
          column(6,
            p("Scroll down to begin", style="color: white; font-size: 2.1vw; font-family: 'IBM Plex Mono', sans-serif;")
          )
        )
      ),
      
      
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
          div(id="begin"),
          br(),
          
          includeScript('js-assets/intersectionObserver/render_on_view1.js'),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br()
        ),

        scrolly_container(
          "scrcon_nattrend_agg", 
          scrolly_graph(
            br(),
            br(),
            #textOutput("section"),
            br(),
            HTML('<center>'),
            highchartOutput("plot__pfa_scr_nattrend_agg",  height = '430px'),
            HTML('</center>')
          ), 
          scrolly_sections(
            HTML('<center>'),
            scrolly_section(
              id = "year",
              br(),
              br(),
              h3("Stop and search over time"), #
              shinyWidgets::sliderTextInput(
                inputId="year_range", label="Which year(s) would like to visualise?",
                choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]),
                force_edges=TRUE
              ),
            ),
            scrolly_section(
              id = "legislation",
              h3("Legislation"),
              p("dit is een paragraaf, die de grafiek rood maakt")
            ),
            scrolly_section(
              id = "reasonForSearch","Reason for search"),
            scrolly_section(
              id = "outcome","Outcome of search"),
            scrolly_section(id = "buffer_bottom", br()),
            HTML('</center>'),
          )
        )
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