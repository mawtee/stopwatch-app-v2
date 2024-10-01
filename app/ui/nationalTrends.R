

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
                   icon("fas fa-chevron-down", "fa-3x", style = "color: #ffffff; margin-left: 30px; margin-bottom: 5px; size: 5vh") 
                 ),
          ),
          column(6,
                 p("Scroll down to begin", style="color: white; font-size: 2.1vw; font-family: 'IBM Plex Mono', sans-serif;")
          )
        )
      ),
      
      
      conditionalPanel(
        condition = "input.pageview_toggle_nattre == 0",
        div(class='section-buffer'),
        div(id='natDASH',
            div(id='natDASH-buffer'),
            div(id='natDASH-contents',
            fluidRow(
              column(3, align='left',
                     div(style='height:2vh'),
                    
                     # starts here
                     #p('Chart Builder', style='font-size:4vh;font-family: Public Sans Thin, sans-serif;'),
                     tags$head(tags$style(HTML("#xaxis_dash + div > .form-group shiny-input-container {height: .5vh; font-size.5vh}"))),
                     #new one here
                     column(8,
                      shinydashboardPlus::box(
                       title = 
                         span(
                         span("Chart Builder",  style = 'font-size:3vh;font-family: Public Sans Thin, sans-serif; color = "#333333"; font-weight: bold',
                              span(fontawesome::fa("info-circle", a11y = "sem", fill_opacity=.7, title='Build your own chart using the selectors below'),style = 'font-size:2.4vh; vertical-align: 1.6vh')
                           #fontawesome::fa("info-circle", a11y = "sem", fill='#e10000', title='Build your own chart using the selectors below'),
                         ) # https://shiny.posit.co/blog/posts/bslib-tooltips/
                       ),
                       collapsible = TRUE,  width = NULL, id='box-1',
                       
                       #),
                       pickerInput(
                         inputId = "yaxis_dash",
                         label = " Select metric",
                         choices=c("Number of stop-searches", "Stop-search rate", 'Ethnic disparities', 'Arrest rate'),
                         selected=c("Number of searches"),
                         multiple = FALSE
                       ),
                       pickerInput(
                         inputId = "xaxis_dash",
                         label = "Select x-axis",
                         choices=c("Year"='year', "Police Force Area"='pfaName', "Ethnic group"='selfDefinedEthnicGroup', "Ethnicity"='selfDefinedEthnicity', "Legislation"='legislation', "Reason for search"='reasonForSearch', "Outcome of search"='outcome'),
                         selected=c("Year"),
                         multiple = FALSE
                       ),
                       pickerInput(
                         inputId = "grouping_dash",
                         label = "Select grouping",
                         choices=c('No grouping'='',  "Year"='year', "Police Force Area (PFA)"='pfaName',  "Ethnic group"='selfDefinedEthnicGroup', "Ethnicity"='selfDefinedEthnicity', "Legislation"='legislation', "Reason for search"='reasonForSearch', "Outcome of search"='outcome'),
                         selected=c("No grouping"),
                         multiple = FALSE
                       )
                     ),
                     
                     
                    # div(style='height:5vh'),
                     #p('Chart Filters', style='font-size:3vh;font-family: Public Sans Thin, sans-serif;'),
                     
                     #new one here
                    shinydashboardPlus::box(
                      title = 
                        span(
                          span("Chart Filter",  style = 'font-size:3vh;font-family: Public Sans Thin, sans-serif; color = "#333333"; font-weight: bold',
                               span(fontawesome::fa("info-circle", a11y = "sem", fill_opacity=.7, title='Add filters to your chart to obtain more granular insight'),style = 'font-size:2.4vh; vertical-align: 1.6vh')
                          ) # https://shiny.posit.co/blog/posts/bslib-tooltips/
                        ),
                       solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = NULL, id='box-2',
                     shinyWidgets::sliderTextInput(
                       inputId="year_range_dash", label="Which year(s) would like to visualise?",
                       choices=levels(df_pfa$year), selected=c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))]),
                       force_edges=TRUE
                     ),
                     div(style='height:1.5vh'),
                     pickerInput(
                       inputId = "pfa_filter_dash",
                       label = "Police Force Area (PFA)",
                       choices=c(unique(df_pfa$pfaName)),
                       selected=c(unique(df_pfa$pfaName)), options = list(
                         `actions-box` = TRUE, `virtual-scroll`=T, `live-search`=T,
                         `live-search-normalize`=T, `live-search-style`='startsWith'), 
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "ethnicgroup_filter_dash",
                       label = " Ethnic group",
                       choices=c(unique(df_pfa$selfDefinedEthnicGroup)),
                       selected=c(unique(df_pfa$selfDefinedEthnicGroup)), options = list(
                         `actions-box` = TRUE, `virtual-scroll`=T, `live-search`=T,
                         `live-search-normalize`=T, `live-search-style`='contains'), 
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "ethnicity_filter_dash",
                       label = " Ethnicity",
                       choices=c(unique(df_pfa$selfDefinedEthnicity)),
                       selected=c(unique(df_pfa$selfDefinedEthnicity)), options = list(
                         `actions-box` = TRUE, `virtual-scroll`=T, `live-search`=T,
                         `live-search-normalize`=T, `live-search-style`='contains'), 
                       multiple = TRUE
                     ),
                     
                     pickerInput(
                       inputId = "legislation_filter_dash",
                       label = " Legislation",
                       choices=c(unique(df_pfa$legislation)),
                       selected=c(unique(df_pfa$legislation)),
                       options = list(
                         `actions-box` = TRUE, `virtual-scroll`=T, `live-search`=T,
                         `live-search-normalize`=T, `live-search-style`='contains'),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "reason_filter_dash",
                       label = " Reason for search",
                       choices=c(unique(df_pfa$reasonForSearch)),
                       selected=c(unique(df_pfa$reasonForSearch)),
                       options = list(
                         `actions-box` = TRUE, `virtual-scroll`=T, `live-search`=T,
                         `live-search-normalize`=T, `live-search-style`='contains'),
                       multiple = TRUE
                     ),
                     pickerInput(
                       inputId = "outcome_filter_dash",
                       label = " Outcome of search",
                       choices=c(unique(df_pfa$outcome)),
                       selected=c(unique(df_pfa$outcome)),
                       options = list(
                         `actions-box` = TRUE, `virtual-scroll`=T, `live-search`=T,
                         `live-search-normalize`=T, `live-search-style`='contains'),
                       multiple = TRUE
                     )
                     ),
                     #p('Chart Options', style='font-size:4vh;font-family: Public Sans Thin, sans-serif;'),
                    # shinydashboardPlus::box(
                    #   title = 
                    #     span(
                    #       span("Chart Options",  style = 'font-size:3vh;font-family: Public Sans Thin, sans-serif; color = "#333333"; font-weight: bold',
                    #            span(fontawesome::fa("info-circle", a11y = "sem",  fill_opacity=.7, title='Further customise your chart using the options below'),style = 'font-size:2.4vh; vertical-align: 1.6vh')
                    #       ) # https://shiny.posit.co/blog/posts/bslib-tooltips/
                    #     ),
                    #    solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = NULL, id='box-3',
                    #      pickerInput(
                    #        inputId = "legislation_group_dashh",
                    #        label = " Select Legislation",
                    #        choices=c(unique(df_pfa$legislation)),
                    #        selected=c(unique(df_pfa$legislation)),
                    #        options = list(
                    #          `actions-box` = TRUE),
                    #        multiple = TRUE
                    #      ),
                    #      pickerInput(
                    #        inputId = "reason_group_dashh",
                    #        label = " Select Reason for Search",
                    #        choices=c(unique(df_pfa$reasonForSearch)),
                    #        selected=c(unique(df_pfa$reasonForSearch)),
                    #        options = list(
                    #          `actions-box` = TRUE),
                    #        multiple = TRUE
                    #      ),
                    #      pickerInput(
                    #        inputId = "outcome_group_dashh",
                    #        label = " Select Outcome of Search",
                    #        choices=c(unique(df_pfa$outcome)),
                    #        selected=c(unique(df_pfa$outcome)),
                    #        options = list(
                    #          `actions-box` = TRUE),
                    #        multiple = TRUE
                    #    )
                    #   )
              )),
              column(8,
                fluidRow(
                  column(12,
                         shinycssloaders::withSpinner(
                    highchartOutput("dashboard_chart", height='65vh', width='65vw'),
                    color = '#e10000'
                         )
                  )
                ),
                div(style='height:2.5vh'),
                fluidRow(
                  column(5,
                    shinydashboardPlus::box(
                      title = 
                        span(
                          span("Download Chart",  style = 'font-size:3vh;font-family: Public Sans Thin, sans-serif; color = "#333333"; font-weight: bold',
                               span(fontawesome::fa("info-circle", a11y = "sem",  fill_opacity=.7, title='Download an image of your chart'),style = 'font-size:2.4vh; vertical-align: 1.6vh')
                          ) # https://shiny.posit.co/blog/posts/bslib-tooltips/
                        ),                      solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = NULL, id='box-4',
                      div(style=' display:flex; flex-direction: row; justify-content: space-around; align-items: center; ',
                      column(2, #style='width:20%;',
                      radioButtons("download_chart_format", "Format", 
                                   choices = c("PNG" = ".png",
                                               "PDF" = ".pdf"),
                                   inline = F)
                      ),
                      column(6,
                             textInputIcon(
                               inputId = "download_chart_name",
                               label = 'Name',
                               value="",
                               placeholder='Your chart',
                               icon = list(NULL, '.png'),
                               size='sm'
                             ),
                      ),
                      column(4,
                      downloadButton("download_chart", "Download", style='font-size:.85vw')
                      )
                      )
                    )
                  ),
                  div(style='width: .5vw'),
                  column(5,
                    shinydashboardPlus::box(
                      title = 
                        span(
                          span("Download Data",  style = 'font-size:3vh;font-family: Public Sans Thin, sans-serif; color = "#333333"; font-weight: bold',
                               span(fontawesome::fa("info-circle", a11y = "sem",  fill_opacity=.7, title='Export the data used in your chart'),style = 'font-size:2.4vh; vertical-align: 1.6vh')
                          ) # https://shiny.posit.co/blog/posts/bslib-tooltips/
                        ),   
                      solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = NULL,id='box-4',
                      div(style=' display:flex; flex-direction: row; justify-content: space-around; align-items: center',
                          column(2,
                                 radioButtons("download_data_format", "Format", 
                                              choices = c( "XLSX" = ".xlsx",
                                                           "CSV" = ".csv"
                                                         ),
                                              inline = F)#,
                                 #tags$script("$(\"input:radio[name='download_data_format'][value='.xlsx']\").parent().css('color', '#e10000');")

                          ),
                          column(6,
                                 textInputIcon(
                                   inputId = "download_data_name",
                                   label = 'Name',
                                   value="",
                                   placeholder='Your data',
                                   icon = list(NULL, ".xlsx"),
                                   size='sm'
                                 ),
                          ),
                      column(4,
                      downloadButton("download_data", "Download",  style='font-size:.85vw')
                    )
                      )
                  )
                ),
                div(style='width: .5vw'),
                #https://stackoverflow.com/questions/49061057/create-downloadbutton-that-can-output-multiple-file-types-r-shiny
                column(2#,
                # actionBttn(
                #   inputId = "guided_tour",
                #   label = "Guided Tour",
                #   color = "success",
                #   style = "material-flat",
                #   icon = icon("sliders"),
                #   block = TRUE
                # )
                )
              )
            ),
            column(1, style='width: 5vw')
          )
          ),
          div(id='natDASH-footer')
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
          div(class='section-buffer'),
          div(id='natS2',
            div(id='natS2-buffer'),
            div(id='natS2-contents',
                column(
                  5,
                  div(
                    id='natS2-contents-phase2',
                    
                    fluidRow(
                      div(id='natS2-contents-phase2-textA')
                    ),
                    fluidRow(
                      div(id='natS2-contents-phase2-textB')
                    ),
                    fluidRow(
                      div(id='natS2-contents-phase2-item', style='height:36vh')
                    ),
                    fluidRow(
                      div(id='natS2-contents-phase2-textC')
                    )
                  )
                ),
                column(
                  7, 
                  div(
                    id='natS2-contents-phase3' ,style='margin-left: 2.5vw;margin-right:.5vw;',
                    fluidRow(
                      div(id='natS2-contents-phase3-text')
                    ),
                    fluidRow(
                      shinyjs::hidden(
                      div(id='natS2-contents-phase3-mirror',
                        uiOutput('plot.ui')
                        )
                      )
                    )
                  ),
                  div(
                    id='natS2-contents-phase4',style='margin-left: 2.5vw;margin-right:.5vw;margin-top: 2vh;',
                    fluidRow(
                      div(id='natS2-contents-phase4-text')
                    ),
                    fluidRow(
                      column(
                        11,
                        div(id='natS2-contents-phase4-line')
                      ),
                      column(
                        1,
                        div(id='natS2-contents-phase4-scroll')
                      )
                    )
                  )
                )
            ),
              div(id='natS2-footer'),
              
          ),
          
          conditionalPanel(
            condition =  "input.render__natS3_cond_panel > 0",
            div(class='section-buffer'),
            div(id='natS3',
                div(id='natS3-buffer'),
                div(id='natS3-contents',
                  column(
                    6,
                    div(id='natS3-contents-phase2',
                      fluidRow(
                        div(id='natS3-contents-phase2-text', style='height:20vh')
                      ),
                      fluidRow(
                        div(id='natS3-contents-phase2-bubble')
                      )
                    )
                  ),
                  column(
                    6,
                    div(id='natS3-contents-phase3',
                      fluidRow(
                        div(id='natS3-contents-phase3-text', style='height:20vh')
                      ),
                      fluidRow(
                        div(id='natS3-contents-phase3-square')
                      )
                    )
                  )
                ),
                div(id='natS3-footer')
            ),
            
            conditionalPanel(
              condition =  "input.render__natS4_cond_panel > 0",
              div(class='section-buffer'),
              div(class='section-buffer'),
            
            fluidRow(#style='background-color:#eeeeee;',
              column(5,
                     div(id='head', style='height:3vh'),
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
            )
            )
            
                
          
        )
        )
      ),
        

      includeScript('js-assets/intersectionObserver/render_on_view.js')
      
    )
  )
)

