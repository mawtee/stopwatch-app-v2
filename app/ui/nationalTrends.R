

nationalTrends <- (
  
  
  # Create National Trends tabPanel and link to Home 
  #========================================================
  tabPanel(
    value = "National Trends:",
    div(div(class="fa fa-line-chart", role = "navigation"), "National Trends"), # TO-FIX: fa-line chart not rendering
    useShinyjs(),
    fluidPage(
      #style='background-color:#eeeeee;',
      
      
      # Landing page
      #-------------------------------------------------------------------------
      
      div(
        class='landing-page',
        fluidRow(
          br(),
          br(),
          div(class="landing-page-title", 
              span("National Trends")
          )
        ),
        fluidRow(
          div(
            class="landing-page-subtitle",
            span("A high-level view of stop and search across England and Wales"),
            br(),
          )
        ),
        fluidRow(
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
        ),
        fluidRow(
          column(1,
                 conditionalPanel(
                   condition = "input.pageview_toggle_nattre > 0",
                   icon("fas fa-chevron-down fa-bounce", "fa-3x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px;")
                 ),
                 conditionalPanel(
                   condition = "input.pageview_toggle_nattre == 0",
                   icon("fas fa-chevron-down", "fa-6x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px;") 
                 ),
          ),
          column(6,
                 p("Scroll down to begin", style="color: white; font-size: 2.1vw; font-family: 'IBM Plex Mono', sans-serif;")
          )
        )
      ),
      
      # Intro page
      #-------------------------------------------------------------------------
      
      # Scrolly 
      conditionalPanel(
        condition = "input.pageview_toggle_nattre > 0",
        div(id='natS1',
            div(id='natS1-buffer'),
            fluidRow(
              column(6,  
                     div(
                       id='natS1-contents'
                     )
              ),
              column(6,
                     div(
                       id='natS1-contents-phase2', style='height:65vh; margin-left: 5.5vw;margin-right:.5vw'
                     ),
                     
                     div(
                       id='natS1-contents-phase3',  style='height:15vh; margin-left: 5.5vw;margin-right:.5vw;margin-top: 2vh;',
                       div(id='natS1-contents-phase3-text', style='height:8vh'),
                       div(style='height:.5vh'),
                       fluidRow(
                         column(4,
                                shinyjs::hidden(
                                  shinyWidgets::sliderTextInput(
                                    inputId="year_range", label="",
                                    choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]), force_edges=TRUE
                                  )
                                )
                         ),
                         column(4,
                                shinyjs::hidden(
                                  actionButton("year_range_confirm", "Confirm")
                                )
                         )
                       )
                     )
              )
            ),
            fluidRow(
              div(
                id='natS1-footer',
                # TODO Change from condition panel to event-based insertUI
                conditionalPanel(
                  condition =  "input.year_range_confirm > 0",
                  div(style='float:right; position: relative; top: -10vh',
                      icon("fas fa-chevron-down fa-bounce", "fa-3x", style = "color: #333333; margin-right: 5vw; margin-bottom: -2vh;")
                  )
                )
              )
            ) # AT SAME POINT WORK OUT HOW TO RENDER ONCE bottom of intro page has fully left the viewport
        ),
        
        
        # Section Two
        #=======================================================================
        conditionalPanel(
          condition =  "input.year_range_confirm > 0",
          div(id='natS2',
              div(id='natS2-buffer'),
              div(id='natS2-contents'),
              div(id='natS2-footer'),
              
          )
        ),
        
        includeScript('js-assets/intersectionObserver/render_on_view.js')
        
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
      )
    )
  )
)
