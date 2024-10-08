

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
      #style='background-color:#eeeeee;',
      
      
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
              icon("fas fa-chevron-down", "fa-6x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px;") 
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
        div(style="height: 100vh",
            div(style='height: 3vh'),
    

        # TODO add selector for years, and make subsequent stuff into conditional panel
        # e.g. Which years would you like visualise: All, Custom Selection
        # TODO position two new plots across in same col, with separate column for text and then another bigger one for timeline
        # TODO possibly see if can execute render sequentially
        # fluidRow(
        #   column(2,
        #   selectizeInput(
        #     inputId = "cond",
        #     label = tags$span(style="color: #333333;","conditional panel example"),
        #     choices = c("show", "hide"),
        #     multiple = T,
        #     options = list(
        #       placeholder = 'Please select an option below',
        #       maxItems = 1
        #       # onInitialize = I('function() { this.setValue(null); }')
        #     )
        #   )
        #   ), 
        #   column(2,
        #     actionButton("btn", "Confirm selection")
        #   )
        # ), 
        div(style="height:80vh",
        fluidRow(
          div(id = "end", textOutput("yo")),# this is where ui renders
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          # br(),
          fluidRow(
            column(2,
              actionButton("btn", "Confirm selection")
            )
          ),
          div(id="begin", h2("yo")), # marker div for activation of animation


          
          
          includeScript('js-assets/intersectionObserver/render_on_view1.js'), # AT SAME POINT WORK OUT HOW TO RENDER ONCE bottom of intro page has fully left the viewport
          


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
        ),
        column(6,align="center",
               shinyWidgets::sliderTextInput(
                 inputId="year_range", label="Which year(s) would like to visualise?",
                 choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]),
                 force_edges=TRUE
               )
        )
        ),
        br(),
        br(),
        br(),
        br(),
        
        #div(class=)
        
        scrolly_container(
          "scrcon_nattrend_agg",
         # div(id="temp", class="highchart html-widget html-widget-output shiny-report-size html-fill-item-overflow-hidden html-fill-item shiny-bound-output header",
         # div(class="header-wrapper" ,

         # ),
          scrolly_graph( 
            div(style="height: 10vh"),
            fluidRow(
              column(12,
            #div(uiOutput('metric_list'), style = 'font-size: 80%'),

            HTML('<center>'),
            div(sstyle="height:30vh"),
            tagAppendAttributes(
              div(
              shinyWidgets::sliderTextInput(
                inputId="year_range_scr", label="Press play to view by year",
                choices=levels(df_pfa$year), selected=levels(df_pfa$year)[1],
                animationOptions(interval=3500, loop=T)
              ),
              style="position: absolute; width: 18vw; bottom: 6vh; right: 2vw; z-index:1"
              )
            ),
            #div(wellPanel(h1("panel")), style="width=100%", height="10%"),
              div(
                highchartOutput(
                  "plot__pfa_scr_nattrend_agg", height = '100%'
                  ),
                style = "height: 65vh; margin: auto; padding-top: 2vh;"
                ),
           #div(wellPanel(h1("panel")), style="width:100%; height:40vw;"),
           
           # div(style="height:4vh"),
            # 
            # div(class="parent",
            #     div(class="divy", HTML("<b>Ethnic Group</b><br>Filter: x")),
            #     div(class="divy", HTML("<b>Region</b><br>Filter: x")),
            #     div(class="divy", HTML("<b>Legislation</b><br>Filter: x")),
            #     div(class="divy", HTML("<b>Reason for Search</b><br>Filter: x")),
            #     div(class="divy", HTML("<b>Outcome of Search</b><br>Filter: x")),
            #     
            #     style="width:95%; height:20vh;"),
           
           # uiOutput("sidebar_ui"),
            
            HTML('</center>')
            
          )#,
          # column(3, 
          #   wellPanel(
          #     uiOutput("sidebar_ui"),
          #   #uiOutput("sidebar_ui"),,
          #   
          # 
          # 
          #   
          #   
          #   style="width: 98%; height: 90%"))
          # column(4,
          #   div(style="height: 20vh"),
          #   sidebarPanel(id="sidebar", width=12,
          #                shinyWidgets::sliderTextInput(
          #                  inputId="year_range_scr", label="Press play to view by year",
          #                  choices=levels(df_pfa$year), selected=levels(df_pfa$year)[1],
          #                  animationOptions(interval=3500, loop=T)
          #                ),
          #                
          #                )
          #   )
            ),
          width="63.5%"#,
          #style="margin-left:0.5vw" # need to div it
          ),
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
            div(scrolly_section(id = "buffer1", render_text(0), br())),
            div(
              scrolly_section(
                id = 'selfDefinedEthnicityGroup',
                render_text(2), 
                # shinyWidgets::multiInput(
                #   inputId = "ethnic_group_scr", label = "Select ethnicity",
                #   choices=unique(df_pfa$selfDefinedEthnicityGroup),
                #   selected = unique(df_pfa$selfDefinedEthnicityGroup) ,
                # ),
                # selectInput(
                #   inputId="ethnic_group_scr",
                #   label="Select ethnicity",
                #   choices=unique(df_pfa$selfDefinedEthnicityGroup),
                #   selected = unique(df_pfa$selfDefinedEthnicityGroup) ,
                #   multiple = T
                # ),
                pickerInput(
                  inputId = "ethnic_group_scr",
                            label = " Select ethnicity",
                            choices=c(unique(df_pfa$selfDefinedEthnicityGroup)),
                            selected=c(unique(df_pfa$selfDefinedEthnicityGroup)), options = list(
                              `actions-box` = TRUE), 
                  multiple = TRUE
                ),
                # selectizeInput('season', "", choices = shots$SeasonNr, selected = TRUE, multiple = TRUE),
              ) ,style = "height: 125vh;margin-top: 45vh;"),
            # div(
            #   scrolly_section(
            #     id = 'region', render_text(3),
            #     pickerInput(
            #       inputId = "region_group_scr",
            #       label = " Select region",
            #       choices=c(unique(df_pfa$region)),
            #       selected=c(unique(df_pfa$region)),
            #       options = list(
            #         `actions-box` = TRUE), 
            #       multiple = TRUE
            #     )
            #   ),
            #   style = "height: 125vh;margin-top: 45vh;"
            # ),
            div(
              scrolly_section(
                id = 'legislation', render_text(4),
                pickerInput(
                  inputId = "legislation_group_scr",
                  label = " Select legislation",
                  choices=c(unique(df_pfa$legislation)),
                  selected=c(unique(df_pfa$legislation)),
                  options = list(
                    `actions-box` = TRUE), 
                  multiple = TRUE
                )
              ),
              style = "height: 125vh;margin-top: 45vh;"
            ),
            div(
              scrolly_section(
                id = 'reasonForSearch', render_text(5),
                pickerInput(
                  inputId = "reason_group_scr",
                  label = " Select reason",
                  choices=c(unique(df_pfa$reasonForSearch)),
                  selected=c(unique(df_pfa$reasonForSearch)),
                  options = list(
                    `actions-box` = TRUE), 
                  multiple = TRUE
                )
              ),
              style = "height: 125vh;margin-top: 45vh;"
            ),
            div(
              scrolly_section(
                id = 'outcome', render_text(6),
                div(style="height:1vh",
                pickerInput(
                  inputId = "outcome_group_scr",
                  label = " Select outcome",
                  choices=c(unique(df_pfa$outcome)),
                  selected=c(unique(df_pfa$outcome)),
                  options = list(
                    `actions-box` = TRUE), 
                  multiple = TRUE
                )
                )
              ),
              style = "height: 125vh;margin-top: 45vh;"
            ),
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
            HTML('</center>'),
            width="32.5%"#,
            #style="margin-left:0.5vw" # need to div it
            
         # )
          #div(id="footer", wellPanel(h2("footer"), style="width: 90%"), style="position: sticky"), 
          
        ),
        div(class="header", 
            br(),
            br(),
            uiOutput("sidebar_ui")
            # br(),
            # br(),
            # br(),
            # br(),
            # br(),
        )
        )
        ,
        
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
        
        fluidRow(#style='background-color:#eeeeee;',
          column(5,
            div(style='height:3vh'),
            div(style="position: absolute; left: 3vw; align-items: left;",
            # pickerInput(
            #   inputId = "pfa_select",
            #   label = "Select a Police Force to visualise",
            #   choices = unique(df_pfa$pfaName),
            #   selected=character(0),
            #   options = list(
            #     `actions-box` = T,
            #     `live-search` = T,
            #     onInitialize = I('function() { this.setValue(""); }')
            #   )
            # ),
            selectizeInput(
              inputId = "pfa_select",
              label = tags$span(style="color: #333333;","Select a Police Force to visualise"),
              choices = unique(df_pfa$pfaName),
              multiple = T,
                options = list(
                placeholder = 'Please select an option below',
                maxItems = 1
               # onInitialize = I('function() { this.setValue(null); }')
              )
            ),
            div(id='countUp-pfa-ui'),
            #div(style='height:5vh'),
           # h4("plot goes here")
            #div(highchartOutput('column__pfa_scr_nattrend_agg'), style='height: 1vh')
            )
            
            #countupOutput("countUp_pfa")
          ),
          column(7, 
            align='center', highchartOutput("map__pfa_scr_nattrend_agg", height='85vh', width='95%'),
            tagAppendAttributes(
              div(
            dropMenu(
              actionButton("go", "Plot options", icon = icon('plus')),
              tags$h3("Some inputs"),
              sliderInput(
                "obs", "Number of observations:",
                min = 0, max = 1000, value = 500
              ),
              selectInput(
                "variable", "Variable:",
                c("Cylinders" = "cyl",
                  "Transmission" = "am",
                  "Gears" = "gear")
              ),
              pickerInput(
                inputId = "pckr",
                label = "Select all option",
                choices = rownames(mtcars),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              radioButtons(
                "dist", "Distribution type:",
                c("Normal" = "norm",
                  "Uniform" = "unif",
                  "Log-normal" = "lnorm",
                  "Exponential" = "exp")
              )
            ),
            style="position: absolute; top: 20%; right: 12%; z-index:1"
              )
            ),
            conditionalPanel(
              condition =  "input.pfa_select.length > 0",
              div(style='float:right; position: relative; top: -10vh',
                icon("fas fa-chevron-down fa-bounce", "fa-3x", style = "color: #333333; margin-left: 30px; margin-bottom: 5px;")
              )
            )
          )
        ),
        # ),
        # # conditionalPanel(
        # #   #condition =  'paste('[',paste(paste('"',unique(df_pfa$pfaName),'"',sep=''),collapse=','),']').includes(input.pfa_select)',
        # #   #condition = 'input.pfa_select == "Avon and Somerset"',
        # #   #condition = "array1.includes(input.pfa_select)",
        # #  # condition = "input.pfa_select && ['Avon and Somerset', 'Essex', 'Bedfordshire'].indexOf(input.pfa_select) > -1",
        # #  #condition = paste0(paste0("[",toString(paste0("'",df_pfa$pfaName,"'")),"]"),".includes(input.pfa_select)"),
        # #   #condition = "input.pfa_select.indexOf(array1) > -1",
        # #  #condition = paste0("[", unique(df_pfa$pfaName), "]===input.pfa_select"),
        # #  #condition='paste0("[", unique(df_pfa$pfaName), "]===input.pfa_select;',
        # #   #includes(["Avon and Somerset","Essex"])',
        # #   condition =  "typeof input.pfa_select !== 'string'",
        # #   br(),
        # #   br(),
        # #   h2("NULLLL")
        # # ),
        conditionalPanel(
          condition =  "input.pfa_select.length > 0",
           fluidRow(
             column(2,
             h2("PFA SCROLLY!!!!")
             ),
          column(7, 
                 align='center', highchartOutput("map__pfa_pie", height='85vh', width='95%')
          )
           )
          
          #map__pfa_pie
          #   scrolly_container(
          #     "scrcon_nattrend_agg2",
          #     scrolly_graph(
          #       div(style="height: 10vh"),
          #       fluidRow(
          #         column(12,
          #           div(
          #             highchartOutput("plot__pfa_scr_nattrend_agg", height = '100%'),
          #             style = "height: 65vh; margin: auto; padding-top: 2vh;"
          #           )
          #         )
          #       ), width="63.5%",
          #     )
          #   )
          )
          #     #style="margin-left:0.5vw" # need to div it
          #   ),
          #   scrolly_sections(
          #     
          #     HTML('<center>'),
          #     # scrolly_section(id = 0, render_text(0), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 1, render_text(1), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 2, render_text(2), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 3, render_text(3), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 4, render_text(4), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 5, render_text(5), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 6, render_text(6), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 7, render_text(7), br(), br(), br(), br(), br()),
          #     # scrolly_section(id = 8, render_text(8), br(), br(), br(), br(), br()),
          #     
          #     #div(scrolly_section(id = 'year', render_text(1)),style = "height: 85vh;margin-top: 45vh;"),
          #     div(scrolly_section(id = "buffer12", render_text(0), br())),
          #     div(
          #       scrolly_section(
          #         id = 'selfDefinedEthnicityGroup',
          #         render_text(2), 
          #         # shinyWidgets::multiInput(
          #         #   inputId = "ethnic_group_scr", label = "Select ethnicity",
          #         #   choices=unique(df_pfa$selfDefinedEthnicityGroup),
          #         #   selected = unique(df_pfa$selfDefinedEthnicityGroup) ,
          #         # ),
          #         # selectInput(
          #         #   inputId="ethnic_group_scr",
          #         #   label="Select ethnicity",
          #         #   choices=unique(df_pfa$selfDefinedEthnicityGroup),
          #         #   selected = unique(df_pfa$selfDefinedEthnicityGroup) ,
          #         #   multiple = T
          #         # ),
          #         # selectizeInput('season', "", choices = shots$SeasonNr, selected = TRUE, multiple = TRUE),
          #       ) ,style = "height: 125vh;margin-top: 45vh;"),
          #     div(
          #       scrolly_section(
          #         id = 'region', render_text(3),
          #       ),
          #       style = "height: 125vh;margin-top: 45vh;"
          #     )
          #   )
          # )
        # TODO add year-slider on map page
        # TODO link year-slider to map
        # TODO linkmap year-slider to countup, with year text (ignore formatting for now)
        # TODO add metric selectorUI
        # TODO add rate of searches to map plot function
        # TODO adjust count up for rate
        
        # TODO on scrolly show text for all categories, and allow it to vary by year
        # TODO potentially add current page into scrolly container
        

        
        
        # TODO add hc item chart when user switches to rate of stop and search
        # TODO add year slider, to play through years
        # TODO add some option to keep existing filters (drop-down to view existing filters), or refresh, or maybe even edit?
        
      ),
      # here 

conditionalPanel(
  condition = "input.pageview_toggle_nattre == 0",
  
  div(style="height: 100vh",
      div(style='height: 3vh'),
      fluidRow(#style='background-color:#eeeeee;',
        column(3,         
               # starts here
               div(class="red-header",
                   span("Options"),
                   br()),
               pickerInput(
                 inputId = "yaxis_dash",
                 label = " Select Y Axis",
                 choices=c("Number of Searches", "Rate of Searches"),
                 selected=c("Number of Searches"),
                 multiple = FALSE
               ), #Add X and Y 
               pickerInput(
                 inputId = "xaxis_dash",
                 label = "Select X Axis",
                 choices=c("Year", "PFA", "Ethnicity", "Legislation", "Reason for Search", "Outcome of Search"),
                 selected=c("Year"),
                 multiple = FALSE
               ),
               br(),
               br(),#Add X and Y 
               div(class="red-header",
                   span("Filters"),
                   br()),
               shinyWidgets::sliderTextInput(
                 inputId="year_range_dash", label="Which year(s) would like to visualise?",
                 choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]),
                 force_edges=TRUE
               ),
               pickerInput(
                 inputId = "pfa_group_dash",
                 label = "Select PFA",
                 choices=c(unique(df_pfa$pfaName)),
                 selected=c(unique(df_pfa$pfaName)), options = list(
                   `actions-box` = TRUE), 
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "ethnic_group_dash",
                 label = " Select Ethnicity",
                 choices=c(unique(df_pfa$selfDefinedEthnicityGroup)),
                 selected=c(unique(df_pfa$selfDefinedEthnicityGroup)), options = list(
                   `actions-box` = TRUE), 
                 multiple = TRUE
               ),

               pickerInput(
                 inputId = "legislation_group_dash",
                 label = " Select Legislation",
                 choices=c(unique(df_pfa$legislation)),
                 selected=c(unique(df_pfa$legislation)),
                 options = list(
                   `actions-box` = TRUE),
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "reason_group_dash",
                 label = " Select Reason for Search",
                 choices=c(unique(df_pfa$reasonForSearch)),
                 selected=c(unique(df_pfa$reasonForSearch)),
                 options = list(
                   `actions-box` = TRUE),
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "outcome_group_dash",
                 label = " Select Outcome of Search",
                 choices=c(unique(df_pfa$outcome)),
                 selected=c(unique(df_pfa$outcome)),
                 options = list(
                   `actions-box` = TRUE),
                 multiple = TRUE
               )
        ),
        column(7,
               highchartOutput("dashboard_chart"))
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