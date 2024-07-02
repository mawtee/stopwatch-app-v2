# https://shiny.posit.co/r/gallery/government-public-sector/scotpho-profiles/
# https://github.com/Public-Health-Scotland/scotpho-profiles-tool/blob/master/shiny_app/landing-page.html
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(htmlwidgets)
library(tidyverse)
library(highcharter)
library(htmltools)
library(scrollytell)
library(shinyThings)
library(anicon)
library(shiny.fluent)
library(shinybrowser)
library(plotly)
library(geojsonio)
library(countup)
library(manipulateWidget)
library(sf)

source("app/ui/home.R")
source("app/ui/nationalTrends.R")
source("app/funs/postProcessing_funs.R")
source("app/funs/plotting_funs.R")
source("app/funs/countUp_funs.R")
source("app/funs/onOffToggle_funs.R")


df_pfa <- read_csv("data/dfPFA_clean_231009.csv")
df_pfa$financialYear <- ifelse(df_pfa$financialYear=="2020/2021", "2020/21", 
                               ifelse(df_pfa$financialYear=="2021/2022", "2021/22", df_pfa$financialYear))
df_pfa$year <- factor(df_pfa$year,labels=unique(df_pfa$financialYear))
df_pfa <- df_pfa[df_pfa$pfaName != "British Transport Police",]

#library(here)
#source(here("scripts_scr/source_code_for_shiny.R"))
json_pfa <- "data/smoothed.geojson"
bounds_pfa <- geojsonio::geojson_read(json_pfa, what = "list")


sf_use_s2(FALSE)
bounds_pfa__sf <- geojsonsf::geojson_sf(json_pfa)
bounds_pfa__df <- st_as_sf(bounds_pfa__sf, coords = c('long', 'lat'))
bounds_pfa__df <- st_make_valid(bounds_pfa__df) %>% select(long, lat, pfa16nm)
bounds_pfa__df <- bounds_pfa__df[1:3]


# centroids <- bounds_pfa__df %>%
#   group_by(pfa16nm) %>%
#   summarize(geometry = st_union(geometry)) %>%
#   st_centroid %>%
#   st_coordinates() %>%
#   as.data.frame()
# centroids$pfaName <- unique(df_pfa$pfaName)
# names(centroids)[1:2] <- c("lon", "lat")







# Define the UI
#==============================================================================

ui <- 
  tagList(


  useShinyjs(), 
  # Get user browser size for scaling outputs
  shinybrowser::detect(),
  navbarPage(position = c("fixed-top"),

             
    
    
    # Add StopWatch logo to navigation bar
    title = div(
      style = "position: relative; top: -15px; 
               margin-left: 10px; margin-top: 5px;",
      tags$a(
        img(src = "stopwatch_logo.png", width = 120, alt = "hyperlink"),
                   href = "https://www.stop-watch.org/", target = "_blank"
      )
    ),
    windowTitle = "Stop and Search Tracker",
    collapsible = TRUE,
    
   # Declare CSS styles
   includeCSS("www/styles_old.css"),
  
   
   

   
   # Home
   home,

   # National Trends
   nationalTrends,
   
   
   
   
   
   
  
   tabPanel(
     value = "Area profiles",
     div(div(class="fa fa-signal", role = "navigation"), "Area Profiles")
   ),
   ##navbarMenu("Email your Representative", # code handy in case need to do menu thing, but probably just condence option into button instead
   #  tabPanel("Member of Parliament (MP)"),
   #  tabPanel("Police Chief Constable (PCC)")
   #),
   #tabPanel(
   #  value = "Data",
   #  div(div(class="fa fa-list-ul", role = "navigation"), "Data")
  # ),
  tabPanel(
    value = "Data",
    div(div(class="fa-solid fa-scale-unbalanced", role = "navigation"), "Data")
  ),
   tabPanel(
     value = "About",
     div(div(class="fa fa-list-ul", role = "navigation"), "About")
   )
   
   
   
   
   
   # IBM Plex Mono for tagline
   
   

    
  )# close navbarPage
) # close taglist



# #f9f9f9
  
  
#  https://shiny.posit.co/r/gallery/government-public-sector/scotpho-profiles/





# https://stackoverflow.com/questions/26804927/how-to-resize-the-width-of-a-div-based-on-another-div-on-its-right-only-with-c





# Pulling from main
# https://stackoverflow.com/questions/64155911/get-latest-updates-from-master-into-my-branch























