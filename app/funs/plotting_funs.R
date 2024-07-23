colour_list <- c()




plot__nattrend_line <- function(df_pfa, year_range) {
  
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  df_pfa_plot <- df_pfa %>%
    group_by(year) %>%
    mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
    #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
    ungroup() %>%
    distinct(year, .keep_all=T) %>%
    mutate(numberOfSearches_cumsum = cumsum(numberOfSearches)) %>%
    select(year, numberOfSearches, numberOfSearches_cumsum)
  
  
  plot <-
    highchart() %>%
    hc_xAxis(categories = df_pfa_plot$year) %>%
    hc_add_series(
      type='column', name='In-year', data=df_pfa_plot$numberOfSearches, color="#e10000"
    ) %>%
    hc_add_series(
      type='line', name='Cumulative', data=df_pfa_plot$numberOfSearches_cumsum, lineWidth=3, color="#333333", yAxis = 1
    ) %>%
    hc_yAxis_multiples(
      list(title=list(text=""),min=0, labels=list(enabled=F),gridLineWidth=0),
      list(title=list(text=""),min=0, labels=list(enabled=F),gridLineWidth=0)
    ) %>%
    hc_xAxis(
      title=list(text=""),
      labels=list(style=list(fontSize='11'))
    ) %>%
    hc_colors("#e10000") 
  # hc_plotOptions(
  #   'column' = list(
  #     dataLabels = list(
  #       enabled=T,
  #       x=.5, y=.5, crop=F,
  #       overflow= 'none',
  #       style = list(
  #         fontSize = "11",
  #         textOutline = FALSE,
  #         fontWeight='normal'
  #       )
  #     ),
  #     pointPadding = 0
  #   )
  # )
  return(plot)
  
}



plot__nattrend_item <- function(df_pfa, year_range) {
  
  # year_range_int <- which(levels(df_pfa$year) %in% year_range)
  # df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  # df_pfa_plot <- df_pfa %>%
  #   mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
  #   mutate(arrest = case_when(outcome == "Arrest"~1, T~0)) %>%
  #   mutate(numberArrest = sum(arrest)) %>%
  #   summarise(propArrest = (numberArrest/numberOfSearches)*100) %>%
  #   slice(1)
  # 13% result in an arrest
  df_pfa_plot <- data.frame(
    name = c('Arrest', 'No Arrest'),
    value = c(13, 87)
  )
  
  # https://stackoverflow.com/questions/57999667/changing-output-after-delay-in-r-shiny-app
  plot <-
    highchart() %>%
    hc_chart(type='item') %>%
    hc_add_series(data=df_pfa_plot$value) 
  
  # bubble plot
  # 
  # 
  # %>%
  #   hc_add_series(data=df_pfa_plot$nonarrests)
  #     
  #     
  #   )
  #   hc_xAxis(categories = df_pfa_plot$year) %>%
  #   hc_add_series(
  #     type='column', name='In-year', data=df_pfa_plot$numberOfSearches, color="#e10000"
  #   ) %>%
  #   hc_add_series(
  #     type='line', name='Cumulative', data=df_pfa_plot$numberOfSearches_cumsum, lineWidth=3, color="#333333", yAxis = 1
  #   ) %>%
  #   hc_yAxis_multiples(
  #     list(title=list(text=""),min=0, labels=list(enabled=F),gridLineWidth=0),
  #     list(title=list(text=""),min=0, labels=list(enabled=F),gridLineWidth=0)
  #   ) %>%
  #   hc_xAxis(
  #     title=list(text=""),
  #     labels=list(style=list(fontSize='11'))
  #   ) %>%
  #   hc_colors("#e10000") 
  # hc_plotOptions(
  #   'column' = list(
  #     dataLabels = list(
  #       enabled=T,
  #       x=.5, y=.5, crop=F,
  #       overflow= 'none',
  #       style = list(
  #         fontSize = "11",
  #         textOutline = FALSE,
  #         fontWeight='normal'
  #       )
  #     ),
  #     pointPadding = 0
  #   )
  # )
  return(plot)
  
}





# National Trends timeline plot
#===============================================================================
plot__nattrend_timeline <- function(df_pfa, year_range, browser_width, browser_height) {
  
  # INPUTS AND SHIT (IMPORTAMT LINKS)
  #----------------------
  #https://jsfiddle.net/BlackLabel/ytrfd4xn/
  # https://jkunst.com/blog/posts/2021-01-07-minimalistic-toolptips-with-highcharter-and-highcharts/index.html
  
  # Define width of timeline events by number of stop-searches (emulating stacked bar chart)
  #-------------------------------------------------------------------------------------------
  
  fullscreen_width <- 750 # optimal pixel width for full screen timeline view
  total_searches <- sum(df_pfa$numberOfSearches, na.rm=T)
  #year_widths <- df_pfa %>% 
  #  group_by(year) %>% 
  #  summarise(yearWidth = (sum(numberOfSearches, na.rm=T)/total_searches)*fullscreen_width) %>%
  #  ungroup() %>%
  #  select(yearWidth)
  year_widths <- df_pfa %>% 
    group_by(year) %>% 
    summarise(yearWidth = (round(sum(numberOfSearches, na.rm=T)/total_searches,2))*100) %>%
    # mutate(yearWidth = as.character(paste0(yearWidth,"%"))) %>%
    ungroup() %>%
    select(yearWidth)
  year_widths <- c(year_widths$yearWidth*6)
  width <-  0.07*browser_width
  # browser()
  
  #1920
  #1280
  
  # sum(year_widths)
  #year_widths <- rep(80,11)
  #browser()
  # 1210 is full number
  
  # need to find total number searches
  
  # then find each year as %
  
  # then %*1210
  
  # Define colours
  #-----------------------------------------------------------------------------
  pal <- colorRampPalette(rev(c("#e10000", "#fce5e5")))
  colors <- pal(11)
  range01 <- function(x)(x-min(x))/diff(range(x))
  year_colors <- colorRamp(pal(11))(range01(year_widths))
  year_colors <- apply(year_colors, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
  #browser()
  
  
  # Define timeline year list
  #--------------------------
  
  # separate lists = separate labels, rendering sequentially
  
  timeline_list <- list(
    list(
      name ='2011/12',
      label = '<hr>No. stop-searches: <b>500,000</b><br>Policy: <b>Introduction of new policy name</b>',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[1]
    ),
    list(
      name ='2012/13',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[2]
    ),
    list(
      name ='2013/14',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[3]
    ),
    list(
      name ='2014/15',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[4]
    ),
    list(
      name ='2015/16',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[5]
    ),
    list(
      name ='2016/17',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[6]
    ),
    list(
      name ='2017/18',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[7]
    ),
    list(
      name ='2018/19',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[8]
    ),
    list(
      name ='2019/20',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[9]
    ),
    list(
      name ='2020/21',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[10]
    ),
    list(
      name ='2021/22',
      label = '<b>500,000</b> stop-searches<br>Introduction of x ',
      description = 'Description of this event.',
      marker = list(width=width),
      color=year_colors[11]
    )
  )
  
  # 1210
  # Filter timeline year list by select years 
  #--------------------------------------------
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  if (length(year_range_int)==1) {years_list <- levels(df_pfa$year)[year_range_int[1]] }
  if (length(year_range_int)>1) { years_list <- levels(df_pfa$year)[year_range_int[1]:year_range_int[2]]}
  timeline_list_select <- timeline_list[which(sapply(timeline_list, `[[`, 1) %in% years_list)]
  timeline_anim_duration <-  455*length(timeline_list_select) # duration for full 11-year series = 5 seconds -> (5000/11=455)
  #browser()
  
  # single series for eeach year = sequential labels
  # Plot timeline
  #--------------------------------------------
  plot <- highchart() %>%
    hc_add_dependency("modules/timeline.js") %>%
    hc_chart(type="timeline") %>% #, inverted=T
    hc_xAxis(visible=F) %>%
    hc_yAxis(visible=F) %>%
    hc_title(text="") %>%
    hc_add_series(
      dataLabels = list(
        enabled = TRUE,
        animation=T,
        connectorColor= 'silver',
        connectorWidth= 2,
        style=list(
          fontSize='0.7em',
          textOverflow= 'clip'
        )
      ),
      data = timeline_list_select
    ) %>%
    # hc_add_series(
    #   dataLabels = list(
    #     enabled = TRUE,
    #     animation=list(
    #       defer=8000),
    #     connectorColor= 'silver',
    #     connectorWidth= 2,
    #     style=list(
    #       fontSize='0.7em',
    #       textOverflow= 'clip'
    #     )
  #   ),
  #   data = timeline_list_select[[2]]
  # ) %>%
  # hc_add_series(
  #   dataLabels = list(
  #     enabled = TRUE,
  #     animation=list(
  #       defer=8000),
  #     connectorColor= 'silver',
  #     connectorWidth= 2,
  #     style=list(
  #       fontSize='0.7em',
  #       textOverflow= 'clip'
  #     )
  #   ),
  #   data = timeline_list_select[[3]]
  # ) %>%
  # hc_add_series(
  #   dataLabels = list(
  #     enabled = TRUE,
  #     animation=list(
  #       defer=8000),
  #     connectorColor= 'silver',
  #     connectorWidth= 2,
  #     style=list(
  #       fontSize='0.7em',
  #       textOverflow= 'clip'
  #     )
  #   ),
  #   data = timeline_list_select[[4]]
  # ) %>%
  # hc_add_series(
  #   dataLabels = list(
  #     enabled = TRUE,
  #     animation=list(
  #       defer=8000),
  #     connectorColor= 'silver',
  #     connectorWidth= 2,
  #     style=list(
  #       fontSize='0.7em',
  #       textOverflow= 'clip'
  #     )
  #   ),
  #   data = timeline_list_select[[5]]
  # ) %>%
  # hc_add_series(
  #   dataLabels = list(
  #     enabled = TRUE,
  #     animation=list(
  #       defer=8000),
  #     connectorColor= 'silver',
  #     connectorWidth= 2,
  #     style=list(
  #       fontSize='0.7em',
  #       textOverflow= 'clip'
  #     )
  #   ),
  #   data = timeline_list_select[[6]]
  # ) %>%
  hc_plotOptions(series=list(
    animation=list(duration=timeline_anim_duration),
    lineWidth=1)) #%>%
  # hc_annotations(animation=list(defer=1000))
  
  #https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/annotations/defer/
  
  
  return(plot)
}


























plot__pfa_scr_nattrend_agg <- function(df_pfa, year_range, year_range_scr, group, ethnic_group, region_group, legislation_group, reason_group, outcome_group) {
  
  if (group != "buffer1") {
    
    #browser()
    if(!is.null(ethnic_group)) {
      df_pfa <- df_pfa %>% filter(selfDefinedEthnicityGroup %in% ethnic_group)
    }
    if(!is.null(region_group)) {
      df_pfa <- df_pfa %>% filter(region %in% region_group)
    }
    if(!is.null(legislation_group)) {
      df_pfa <- df_pfa %>% filter(legislation %in% legislation_group)
    }
    if(!is.null(reason_group)) {
      df_pfa <- df_pfa %>% filter(reasonForSearch %in% reason_group)
    }
    if(!is.null(outcome_group)) {
      df_pfa <- df_pfa %>% filter(outcome %in% outcome_group)
    }
    
    
    
    
    
    #browser()
    
    year_range_int <- which(levels(df_pfa$year) %in% year_range)
    df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
    #browser()
    df_pfa <- df_pfa %>%
      group_by(year, .data[[group]]) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
      ungroup() %>% 
      distinct(year, .data[[group]], .keep_all=T) #%>%
    #select(year, .data[[group]], numberOfSearches) 
    #arrange(.data[[group]]) %>%               # sort your dataframe
    #mutate(group = factor(.data[[group]], unique(.data[[group]]))) # reset your factor-column based on that order
    
    
    
    #browser()
    df_pfa[group] <- eval(parse(text=paste0("factor(df_pfa$",group,")")))
    df_pfa[group] <- eval(parse(text=paste0("fct_rev(reorder(df_pfa$",group,", df_pfa$numberOfSearches, .fun = mean))")))
    
    
    #browser()
    #group_categories <- dplyr::n_distinct(df_pfa[group])
    
    #600000 #e10000 #f29191
    #pal <- colorRampPalette(rev(c("#600000", "#e10000", "#f29191")))
    #colors <- pal(group_categories)
    #range01 <- function(x)(x-min(x))/diff(range(x))
    #year_colors <- colorRamp(pal(group_categories))(range01(df_pfa$numberOfSearches))#(range01(year_widths))
    #year_colors <- apply(year_colors, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
    #browser()
    #browser()
    
    if (nchar(year_range_scr)>8) {
      df_pfa_plot <- df_pfa %>%
        group_by(.data[[group]]) %>%
        summarise(numberOfSearches = sum(numberOfSearches, na.mrm=T)) %>%
        #summarise(numberOfSearches_fmt = case_when(
        #   max(numberOfSearches)>1000000~round(sum(numberOfSearches, na.rm=T)/1000000, 2),
        #   T~round(sum(numberOfSearches, na.rm=T)/1000, 2)
        # )) %>%
        ungroup() %>%
        arrange(-numberOfSearches) 
      max <-  max(df_pfa_plot$numberOfSearches)*1.1
      
    }
    else {
      max <- max(tapply(df_pfa$numberOfSearches, df_pfa[group], max))*1.1
      df_pfa_plot <- df_pfa %>%
        filter(year==year_range_scr) %>%
        # filter(selfDefinedEthnicityGroup %in% ethnic_group) %>%
        arrange(-numberOfSearches) 
    }
    
    df_pfa_plot <- df_pfa_plot %>%
      mutate(format = case_when(max(numberOfSearches) > 1000000~ "M", T~ "k")) %>%
      mutate(numberOfSearches = case_when(format=="M"~ round(numberOfSearches/1000000,1), T~ round(numberOfSearches/1000,0)))
    fmt <- unique(df_pfa_plot$format)
    if (fmt == "M") {
      max <- max/1000000
    }
    else {
      max <- max/1000
    }
    
    plot <- df_pfa_plot %>%
      hchart(
        'bar', hcaes(x=.data[[group]], y=numberOfSearches)
      ) %>%
      hc_yAxis(
        #reversedStacks=F,
        #visible=F,
        title=list(text=""),
        min=0, max=max,
        labels=list(
          format=paste0("{value}",fmt)
        ),
        gridLineWidth=0
        
      ) %>%
      hc_xAxis(title=list(text="")) %>%
      
      hc_colors("#e10000") %>%
      # hc_plotOptions(
      #   'bar' = list(
      #     dataLabels = list(
      #       enabled=T,
      #       format=paste0("{y}",fmt),
      #       x=.5, y=.5,
      #       style = list(
      #         fontSize = "14px", 
      #         textOutline = FALSE,
      #         
      #         color = "#5b5b5b",
    #         fontWeight = "normal"
    #       )
    #     )
    #   )
    # ) %>%
    
    hc_title(
      text= paste0("Number of stop-searches, ",year_range_scr),
      align = "center",style = list(
        fontSize ="18px",color = "#333333", 
        fontFamily = "Arial", fontWeight = "400" )) %>%
      hc_exporting(enabled=T) 
    
    
    
    
    
    
    # 
    # if (length(year_range_scr)>2) {
    #   max <-  df_pfa_plot$numberOfSearches
    # }
    # max <- max(df_pfa_plot$numberOfSearches*)
    
    
    # hc_legend(itemStyle=list(fontSize="1.6vh"))
    
    
    return(plot)
    
  }
  
}






plot__pfa_map <- function(df_pfa, bounds_pfa, year_range, pfa_select) {
  
  #browser()
  #https://jkunst.com/highcharter/reference/hc_add_event_point.html
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  # browser()
  #browser()
  df_pfa <- df_pfa %>%
    group_by(pfaName) %>%
    mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
    #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
    ungroup() %>% 
    distinct(pfaName, .keep_all=T) %>%
    select(pfaName, numberOfSearches) %>%
    filter(pfaName != 'London, City of')
  
  # 
  #browser()
  if (!is.null(pfa_select)) {
    
    
    #df_pfa_sf <- sf::st_as_sf(df_pfa_sf)
    
    min <- min(df_pfa$numberOfSearches)
    max <- sort(df_pfa$numberOfSearches, TRUE)[2]+(.05*max(df_pfa$numberOfSearches))
    
    df_pfa_nonselect <- df_pfa %>% filter(pfaName != pfa_select)
    df_pfa_select <- df_pfa %>% filter(pfaName == pfa_select)
    
    vec <- c()
    for (i in 1:length(bounds_pfa$features)) {
      if (bounds_pfa$features[[i]]$properties$pfa16nm == pfa_select) {
        vec[i] <- 1
      }
      else {
        vec[i] <- 0
      }
    }
    pfa_select_index <- which(vec==1)
    bounds_pfa_select <-   list('type'=bounds_pfa$type, 'features'=list(bounds_pfa$features[[pfa_select_index]]))
    
    #browser()
    #rownames(df_pfa) <- df_pfa$pfaName
    highchart() %>%
      # hc_title(
      #   text = "Number of stop-searches",
      #   align = "left",style = list(
      #     fontSize ="28px",color = "#333333", 
      #     fontFamily = "Arial", fontWeight = "400" 
      #   )
      # ) %>%
      hc_subtitle(text = "") %>%
      # hc_legend(align='left', verticalAlign='top', layout='vertical') %>%
      hc_add_series_map(bounds_pfa, df_pfa_nonselect, name="Number of stop-searches", value = "numberOfSearches", joinBy=c("pfa16nm", "pfaName"),
                        borderColor='#FAFAFA', borderWidth=.1, dataLabels = list(enabled = TRUE, format = "{point.pfaName}",
                                                                                 style=list(fontSize='10'))) %>%
      hc_add_series_map(bounds_pfa_select, df_pfa_select, name="Number of stop-searches", value = "numberOfSearches", joinBy=c("pfa16nm", "pfaName"),
                        borderColor='#323232', borderWidth=2, dataLabels = list(enabled = F, format = "{point.pfaName}",
                                                                                style=list(fontSize='9'))) %>%
      
      
      hc_colorAxis(
        minColor = "#fce5e5",
        maxColor = "#e10000",
        min=min,
        max=max
      ) %>%
      hc_mapNavigation(
        enabled = T, enableMouseWheelZoom = T, enableDoubleClickZoom = F,
        style=list(
          fill='white', color='white'
        ),
        
        buttonOptions = list(
          align='right', x=-110,
          theme=list(
            fill='white', `stroke-width`= 3,
            stroke='#f0f2f4', r=10,
            states=list(
              hover=list(
                fill = '#ced1d6'
              )
            )
          )
          
          
        ) 
        
      )
    
  }
  
  else {
    #browser()
    highchart(type = "map") %>%
      hc_add_series(mapData = bounds_pfa, showInLegend = FALSE,
                    borderColor='#323232', borderWidth=.1, showInLegend=F, dataLabels = list(enabled = TRUE, format = "{point.pfa16nm}",
                                                                                             style=list(fontSize='10'))) %>%
      hc_mapNavigation(
        enabled = T, enableMouseWheelZoom = T, enableDoubleClickZoom = F,
        style=list(
          fill='white', color='white'
        ),
        
        buttonOptions = list(
          align='right', x=-110,
          theme=list(
            fill='white', `stroke-width`= 3,
            stroke='#f0f2f4', r=10,
            states=list(
              hover=list(
                fill = '#ced1d6'
              )
            )
          )
          
          
        )
        
      )
    
    # highchart() %>%
    #   # hc_title(
    #   #   text = "Number of stop-searches",
    #   #   align = "left",style = list(
    #   #     fontSize ="28px",color = "#333333", 
    #   #     fontFamily = "Arial", fontWeight = "400" 
    #   #   )
    #   # ) %>%
    #   hc_subtitle(text = "") %>%
    #   hc_add_series_map(bounds_pfa, df_pfa, name="Number of stop-searches", value = "numberOfSearches", joinBy=c("pfa16nm", "pfaName"),
    #                     borderColor='#323232', borderWidth=.1, showInLegend=F, dataLabels = list(enabled = TRUE, format = "{point.pfaName}",
    #                                                                              style=list(fontSize='10'))) %>%
    #   
    #   hc_mapNavigation(
    #     enabled = T, enableMouseWheelZoom = T, enableDoubleClickZoom = F,
    #     style=list(
    #       fill='white', color='white'
    #     ),
    #     
    #     buttonOptions = list(
    #       align='right', x=-110,
    #       theme=list(
    #         fill='white', `stroke-width`= 3,
    #         stroke='#f0f2f4', r=10,
    #         states=list(
    #           hover=list(
    #             fill = '#ced1d6'
    #           )
    #         )
    #       )
    #       
    #       
    #     )
    #     
    #   )
    #https://jkunst.com/highcharter/articles/maps.html
    
  }
  
  
  
}


# 
# 
# 
# plot__map_scr_nattrend_agg <- function(df_pfa, year_range, year_range_scr_pfa, group) {
#   
#   if (group != "buffer1_pfa") {
#     
#     
#     year_range_int <- which(levels(df_pfa$year) %in% year_range)
#     df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
#     #browser()
#     df_pfa <- df_pfa %>%
#       group_by(year, pfaName, .data[[group]]) %>%
#       mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
#       #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
#       ungroup() %>% 
#       distinct(year,pfaName, .data[[group]], .keep_all=T) #%>%
#     #select(year, .data[[group]], numberOfSearches) 
#     #arrange(.data[[group]]) %>%               # sort your dataframe
#     #mutate(group = factor(.data[[group]], unique(.data[[group]]))) # reset your factor-column based on that order
#     
#     
#     
#     #browser()
#     df_pfa[group] <- eval(parse(text=paste0("factor(df_pfa$",group,")")))
#     df_pfa[group] <- eval(parse(text=paste0("fct_rev(reorder(df_pfa$",group,", df_pfa$numberOfSearches, .fun = mean))")))
#     
#     #### NEW####################################################################
#     
#     df_pfa <- df_pfa %>%
#       group_by(pfaName, .data[[group]]) %>%
#       mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
#       distinct(pfaName, .data[[group]], .keep_all=T)
#     
#     
#     merged <- merge(centroids, df_pfa, by='pfaName')  %>%
#       select(pfaName, lat, long, selfDefinedEthnicityGroup, numberOfSearches) %>%
#       pivot_wider(names_from="selfDefinedEthnicityGroup", values_from="numberOfSearches") %>%
#       rowwise() %>%
#       mutate(numberOfSearches = sum(across(Asian:White, na.rm=T)))
#     
#     
#     
#     merged_geojson <- geojson_json(merged, lat = "lat", lon = "long")
#     
#     hcmap(merged_geojson)
#     
#    map <-  highchart(type = "map") %>%
#       hc_add_series(mapData = bounds_pfa , showInLegend = F) 
#    
#    map %>%
#       hc_add_series(data=merged_geojson, type='mappoint') %>%
#       hc_chart(
#         events = list(
#           load = JS(
#             "function(){
#               var chart = this;
#               var data = chart.series[1].data;
#             
#                         
#             
#             
#               var demColor = 'rgba(74,131,240,0.80)';
#               var repColor = 'rgba(220,71,71,0.80)';
#               var libColor = 'rgba(240,190,50,0.80)';
#               var grnColor = 'rgba(90,200,90,0.80)';
#             
#             const COLUMN_WIDTH = 5;
# 
# Highcharts.seriesType('mapcolumn', 'column', {
#   dataLabels: {
#     enabled: false
#   }
# }, {
#   drawPoints: function() {
#     // Proceed
#     Highcharts.seriesTypes.column.prototype.drawPoints.call(this);
# 
#     // Custom      
#     var series = this,
#       points = series.points,
#       firstSeries = series.chart.series[1];
# 
#     Highcharts.each(points, function(point, index) {
#       var state = firstSeries.points[series.index - 3];
# 			var newX = state.plotX + index * COLUMN_WIDTH - COLUMN_WIDTH;
# 			var newY = state.plotY - point.graphic.attr('height') + 10;
# 
# 			
# 			point.tooltipPos[0] = newX;
# 			point.tooltipPos[1] = newY;
# 			
#        point.graphic.attr({
#         x: newX,
#         y: newY,
# 				width: COLUMN_WIDTH
#       }); 
#     });
#   }
# });
# Highcharts.each(chart.series[1].points, function(state) {
#   chart.addSeries({
# 		xAxis: 1,
# 		yAxis: 1,
#     type: 'mapcolumn',
# 		grouping: false,
#     name: state.id,
#     zIndex: 6, // Keep pies above connector lines
#     showInLegend: false,
#     data: [{
#       x: 0,
#       y: 1000,
#       color: demColor
#     }, {
#       x: 0,
#       y: 700,
#       color: repColor
#     }]
#   }, false);
# });
# 
# chart.redraw();
# }"
# )
# )
# )
#             
#             
#             
#             
#             
#             
#             
#             
#             
#             
#             
#             
#             
# 
#             
#             // Custom      
#     var series = this,
#       points = series.points,
#       firstSeries = series.chart.series[0];
# 
#     Highcharts.each(points, function(point, index) {
#       var pfaName = firstSeries.points[series.index - 3];
# 		
#              
#               chart.series[0].points.forEach(pfaName => {
#                 chart.addSeries({
#                 type: 'pie',
#                 name: pfaName.id,
#                 zIndex: 6, // Keep pies above connector lines
#                 minSize: 15,
#                 maxSize: 55,
#                 onPoint: {
#                 id: pfaName.id,
#                 z: (() => {
#                     const mapView = chart.mapView,
#                         zoomFactor = mapView.zoom / mapView.minZoom;
# 
#                     return Math.max(
#                         chart.chartWidth / 45 * zoomFactor, // Min size
#                         chart.chartWidth /
#                         11 * zoomFactor * pfaName.numberOfSearches 
#                     );
#                 })()
#             },
#             states: {
#               inactive: {
#                 enabled: false
#               }
#             },
#             accessibility: {
#               enabled: false
#             },
#             data: [{
#                 name: 'Democrats',
#                 y: pfaName.Black,
#                 color: demColor
#             }, {
#                 name: 'Republicans',
#                 y: pfaName.White,
#                 color: repColor
#             }
#             }]
#             
#         }, false);
#     });
#              
#              
# 
#     // Only redraw once all pies and connectors have been added
#     //chart.redraw();
#     }"
#     
#     
# 
# )
# )
# )
#           
# #https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/maps/demo/map-pies    
# #    https://www.highcharts.com/blog/tutorials/working-with-highcharts-javascript-syntax-in-r/
# #          
#             
#      #            data: [{
#     name: 'Democrats',
#     y: state.demVotes,
#     color: demColor
#   }, {
#     name: 'Republicans',
#     y: state.repVotes,
#     color: repColor
#   }, {
#     name: 'Libertarians',
#     y: state.libVotes,
#     color: libColor
#   }, {
#     name: 'Green',
#     y: state.grnVotes,
#     color: grnColor
#   }]       
#             
#                     
#             chart.series[0].points.forEach(pfa => {
#         // Add the pie for this state
#         chart.addSeries({
#           type: 'pie',
#           name: pfaName,
#           zIndex: 6, // Keep pies above connector lines
#           minSize: 15,
#           maxSize: 55
#         });
#       });
#       "
#           )
#             
#             
#           )
#         
#       )
#     
#     )
#       
#       
#       
#    # https://stackoverflow.com/questions/42235455/highcharter-setextremes-function-in-r/42439506#42439506
#    # https://jsfiddle.net/gh/get/jquery/3.1.1/highslide-software/highcharts.com/tree/master/samples/maps/demo/map-pies/
#   #https://stackoverflow.com/questions/58240474/highcharter-linked-map-and-line-plot
#       hc_add_series(
#         data=merged_geojson,type = "mappoint" #type='pie',
#         #mapping = hcaes(selfDefinedEthnicityGroup, numberOfSearches)
#         
#       )
#     
#     
#     
#     highchart(type = "map") %>%
#       hc_add_series(mapData = bounds_pfa, showInLegend = FALSE,
#                     borderColor='#323232', borderWidth=.1, showInLegend=F, dataLabels = list(enabled = TRUE, format = "{point.pfa16nm}",
#                                                                                              style=list(fontSize='10'))) %>%
#       hc_add_series(
#         data=dfm, type='pie',
#         mapping = hcaes(selfDefinedEthnicityGroup, numberOfSearches)
#         
#       )
#     
#     #browser()
#     #group_categories <- dplyr::n_distinct(df_pfa[group])
#     
#     #600000 #e10000 #f29191
#     #pal <- colorRampPalette(rev(c("#600000", "#e10000", "#f29191")))
#     #colors <- pal(group_categories)
#     #range01 <- function(x)(x-min(x))/diff(range(x))
#     #year_colors <- colorRamp(pal(group_categories))(range01(df_pfa$numberOfSearches))#(range01(year_widths))
#     #year_colors <- apply(year_colors, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
#     #browser()
#     #browser()
#     
#     if (nchar(year_range_scr)>8) {
#       df_pfa_plot <- df_pfa %>%
#         group_by(.data[[group]]) %>%
#         summarise(numberOfSearches = sum(numberOfSearches, na.mrm=T)) %>%
#         #summarise(numberOfSearches_fmt = case_when(
#         #   max(numberOfSearches)>1000000~round(sum(numberOfSearches, na.rm=T)/1000000, 2),
#         #   T~round(sum(numberOfSearches, na.rm=T)/1000, 2)
#         # )) %>%
#         ungroup() %>%
#         arrange(-numberOfSearches) 
#       max <-  max(df_pfa_plot$numberOfSearches)*1.1
#       
#     }
#     else {
#       max <- max(tapply(df_pfa$numberOfSearches, df_pfa[group], max))*1.1
#       df_pfa_plot <- df_pfa %>%
#         filter(year==year_range_scr) %>%
#         # filter(selfDefinedEthnicityGroup %in% ethnic_group) %>%
#         arrange(-numberOfSearches) 
#     }
#     
#     
#     
#     
#     
#     
#     df_pfa_plot <- df_pfa_plot %>%
#       mutate(format = case_when(max(numberOfSearches) > 1000000~ "M", T~ "k")) %>%
#       mutate(numberOfSearches = case_when(format=="M"~ round(numberOfSearches/1000000,1), T~ round(numberOfSearches/1000,0)))
#     fmt <- unique(df_pfa_plot$format)
#     if (fmt == "M") {
#       max <- max/1000000
#     }
#     else {
#       max <- max/1000
#     }
#     
#     plot <- df_pfa_plot %>%
#       hchart(
#         'bar', hcaes(x=.data[[group]], y=numberOfSearches)
#       ) %>%
#       hc_yAxis(
#         #reversedStacks=F,
#         #visible=F,
#         title=list(text=""),
#         min=0, max=max,
#         labels=list(
#           format=paste0("{value}",fmt)
#         ),
#         gridLineWidth=0
#         
#       ) %>%
#       hc_xAxis(title=list(text="")) %>%
#       
#       hc_colors("#e10000") %>%
#       # hc_plotOptions(
#       #   'bar' = list(
#       #     dataLabels = list(
#       #       enabled=T,
#       #       format=paste0("{y}",fmt),
#       #       x=.5, y=.5,
#       #       style = list(
#       #         fontSize = "14px", 
#       #         textOutline = FALSE,
#       #         
#       #         color = "#5b5b5b",
#     #         fontWeight = "normal"
#     #       )
#     #     )
#     #   )
#     # ) %>%
#     
#     hc_title(
#       text= paste0("Number of stop-searches, ",year_range_scr,"<br>''"),
#       align = "center",style = list(
#         fontSize ="18px",color = "#333333", 
#         fontFamily = "Arial", fontWeight = "400" )) 
#     
#     
#     
#     
#     
#     # 
#     # if (length(year_range_scr)>2) {
#     #   max <-  df_pfa_plot$numberOfSearches
#     # }
#     # max <- max(df_pfa_plot$numberOfSearches*)
#     
#     
#     # hc_legend(itemStyle=list(fontSize="1.6vh"))
#     
#     
#     return(plot)
#     
#   }
#   
# }

















# 
plot__pfa_quintiles <- function(df_pfa, year_range, pfa_select) {
  
  #if (is.null(pfa_select)) {
  #browser()
  # }
  
  if (!is.null(pfa_select)) {
    #browser()
    year_range_int <- which(levels(df_pfa$year) %in% year_range)
    df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
    #browser()
    df_pfa_plot1 <- df_pfa %>%
      group_by(pfaName) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
      ungroup() %>%
      distinct(pfaName, .keep_all=T) %>%
      mutate(numberOfSearches_median = median(numberOfSearches)) %>%
      filter(pfaName == pfa_select) %>%
      uncount(2) %>%
      select(pfaName, contains('numberOfSearches'))
    
    df_pfa_plot1[2, c('pfaName', 'numberOfSearches')] <- c('UK median', df_pfa_plot1[2, 'numberOfSearches_median'])
    df_pfa_plot1 <- df_pfa_plot1[c('pfaName', 'numberOfSearches')]
    
    
    
    
    
    # mutate(quintile = ntile(numberOfSearches, 5)) %>%
    # mutate(tokeep=case_when(pfaName==pfa_select~1, T~0))  %>%
    # group_by(quintile) %>%
    # filter(tokeep==max(tokeep, na.rm=T)) %>%
    # mutate(numberOfSearches_q = nth(numberOfSearches, which.min(abs(numberOfSearches-median(numberOfSearches))))) %>%
    # ungroup() %>%
    # filter(numberOfSearches == numberOfSearches_q) %>%
    # distinct(quintile, .keep_all=T) %>%
    # select(pfaName, numberOfSearches, quintile) %>%
    # arrange(numberOfSearches)
    
    # df_pfa_plot <- df_pfa %>%
    #   group_by(pfaName) %>%
    #   mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
    #   #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
    #   ungroup() %>%
    #   distinct(pfaName, .keep_all=T) %>%
    #   mutate(quintile = ntile(numberOfSearches, 5)) %>%
    #   mutate(tokeep=case_when(pfaName==pfa_select~1, T~0))  %>%
    #   group_by(quintile) %>%
    #   filter(tokeep==max(tokeep, na.rm=T)) %>%
    #   mutate(numberOfSearches_q = nth(numberOfSearches, which.min(abs(numberOfSearches-median(numberOfSearches))))) %>%
    #   ungroup() %>%
    #   filter(numberOfSearches == numberOfSearches_q) %>%
    #   distinct(quintile, .keep_all=T) %>%
    #   select(pfaName, numberOfSearches, quintile) %>%
    #   arrange(numberOfSearches)
    # 
    
    
    
    #
    #browser()
    
    plot1 <- df_pfa_plot1 %>%
      hchart(
        'column', hcaes(x=pfaName, y=numberOfSearches)
      ) %>%
      hc_yAxis(
        #reversedStacks=F,
        #visible=F,
        title=list(text=""),
        min=0, 
        labels=list(
          enabled=F
        ),
        #   format=paste0("{value}",fmt)
        # ),
        gridLineWidth=0
        
      ) %>%
      hc_xAxis(title=list(text=""), labels=list(style=list(fontSize='11'))) %>%
      
      hc_colors("#e10000") %>%
      hc_plotOptions(
        'column' = list(
          dataLabels = list(
            enabled=T,
            x=.5, y=.5, crop=F,
            overflow= 'none',
            style = list(
              fontSize = "11",
              textOutline = FALSE,
              fontWeight='normal'
            )
          ),
          pointPadding = 0
        )
      )
    #         
    #         color = "#5b5b5b",
    #         fontWeight = "normal"
    #       )
    # hc_plotOptions(
    #   'bar' = list(
    #     dataLabels = list(
    #       enabled=T,
    #       format=paste0("{y}",fmt),
    #       x=.5, y=.5,
    #       style = list(
    #         fontSize = "14px", 
    #         textOutline = FALSE,
    #         
    #         color = "#5b5b5b",
    #         fontWeight = "normal"
    #       )
    #     )
    #   )
    # ) %>%
    
    # hc_title(
    #   text= "Number of stop-searches<br>",
    #   align = "center",style = list(
    #     fontSize ="18px",color = "#333333", 
    #     fontFamily = "Arial", fontWeight = "400" )) 
    
    
    df_pfa_plot2 <- df_pfa %>%
      group_by(pfaName) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
      ungroup() %>%
      distinct(pfaName, .keep_all=T) %>%
      select(pfaName, year, contains('numberOfSearches'))  %>%
      mutate(year = "")
    #filter(pfaName != "Metropolitan Police")
    
    
    
    y1 <- df_pfa_plot2$numberOfSearches[df_pfa_plot2$pfaName==pfa_select]
    #browser()
    plot2 <- hcboxplot(
      outliers = FALSE,
      x = df_pfa_plot2$numberOfSearches,
      var = df_pfa_plot2$year,
      name = paste0(year_range[1],"-",year_range[2]),
      color='#ced1d6'
    ) %>%
      hc_title(text = "") %>%
      hc_yAxis(
        title = list(text = ""), gridLineWidth=0,
        labels=list(style=list(fontSize='11'))
      )  %>%
      hc_chart(type = "bar") %>%
      hc_add_series(
        data = df_pfa_plot2 %>% filter(pfaName != "Metropolitan Police"),
        type = "scatter",
        hcaes(x = "year", y = "numberOfSearches")
      ) %>%
      hc_annotations(
        list(
          labelOptions = list(
            backgroundColor = 'white',
            verticalAlign = 'top',
            y=-40,
            style=list(fontSize='12')
          ),
          labels = list(
            list(
              point = list(xAxis = 0, yAxis = 0, x = 0, y = y1),
              text = paste0(pfa_select),
              shape= 'connector', align='top'
            )
          )
        )
      )%>%
      hc_plotOptions(scatter = list(
        color = "#e10000",
        marker = list(
          radius = 3,
          symbol = "circle",
          lineWidth = 1
        )
      ))
    
    
    plot <- combineWidgets(plot1, plot2, nrow=1, colsize=c(1.1,2))
    #browser()
    return(plot)
    
    
    
    
    
    
    # %>%
    #hc_colors('white')
    # ))  %>%
    # hc_plotOptions(scatter = list(jitter = list(x = .06, y = 0)))
    
    
    
    
    
    
    
    
  }
  
}















countUp_pfa <- function(count_to, count_from) {
  
  print(count_to)
  print(as.numeric(count_to))
  count_to <- as.numeric(count_to)
  # browser()
  countup(
    count = count_to,
    start_at = count_from,
    options = NULL,
    duration = 2.5,
    start = TRUE,
    width = NULL,
    height = NULL,
    elementId = 'countUp-pfa'
  )
  
  
  
  
  
}
# 
# mapNavigation: {
#   enabled: true,
#   buttonOptions: {
#     theme: {
#       fill: 'white',
#       'stroke-width': 1,
#       stroke: 'silver',
#       r: 0,
#       states: {
#         hover: {
#           fill: '#a4edba'
#         },
#         select: {
#           stroke: '#039',
#           fill: '#a4edba'
#         }
#       }
#     },
#     verticalAlign: 'bottom'
#   }
# },
# 
# 
# 
# 
# 










plot__pfa_tsline_nat_anim <- function(df_pfa, year_range, legislation_select) {
  
  df_pfa_ts <- process__df_pfa_ts(df_pfa, year_range, legislation_select)
  df_pfa_ts_nat <- df_pfa_ts %>%
    group_by(year, legislation) %>%
    summarise(numberOfSearches = sum(numberOfSearches)) %>%
    ungroup() %>%
    mutate(year_int = factor(year) %>% as.integer()) 
  #browser()
  plot <- ggplot(df_pfa_ts_nat, aes(x=year_int, y=numberOfSearches)) + 
    geom_line(color="#E10000", size=1) +
    scale_x_continuous(limits = c(min(df_pfa_ts_nat$year_int), max(df_pfa_ts_nat$year_int)),
                       breaks = seq(min(df_pfa_ts_nat$year_int), max(df_pfa_ts_nat$year_int), 1),
                       labels=df_pfa_ts_nat$year) +
    labs(x="Year", y="Number of searches") +
    theme_minimal() +
    theme(
      panel.grid.minor.y=element_blank(),
      panel.grid.minor.x=element_blank(),
      panel.grid.major.x=element_blank()
    ) +
    transition_reveal(year_int)
  
  animplot <- anim_save("outfile.gif", animate(plot)) # New
  
  return(animplot)
  
}


render_text <- function(num){
  
  div(
    text(num), class = "text"
  )
  
}

text <- function(num){
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5,
           text6,
           text7,
           text8,
           text9,
           text10
    )
  )
}


text2 <- HTML("<H2>Ethnic disparties</H2>
              <br> <p> White people are far less likely to be stopped and searched in relation to people from all other Ethnic Groups. The likelyhood of being searched is X%, compared to a chance of X% being search for any other Ethnic Group.")

text3 <- HTML("<H2>Regional disparities</H2>
              <br> <p>People from X area more X times more likely to be stop and searched, in comparision to the rest of the country. <p>")

text4 <- HTML("<H2>Legislation</H2>
              <br><p>Section 1 (PACE) Section 44/47a (TACT) is the most likely legislation for being stopped and searched. With over X amount of searched compared to all other legislations combined<p>")

text5 <- HTML("<H2>Reason for search</H2>
             <br> <p>Drugs is the most likely reason for being stopped and searched. With over X amount of searched compared to all other regions combined.<p>")

text6 <- HTML("<H2>Outcome of search</H2>
              <br> <p>The most likely outcome of being stopped and searched would be Arrest, with X amount of searches resulting in an arrest, a percentage of X%<p>")

text7 <- HTML("<H2> Master's degrees </H2>
              <br> <p>Workers with <font color='#5E4FA2'>master's degrees</font> have a median income of $69,732.
              <br> On average, those occupations have a <b>10% chance</b> of job automation.
              <br><br> There are 1,281,710 workers with a <font color='#5E4FA2'>master's degree</font>.<p>")

text7 <- HTML("<H2> Doctoral degrees </H2>
              <br> <p>Workers with <b>doctoral degrees</b> have a median income of $84,396.
              <br> On average, those occupations have a <b>3% chance</b> of job automation.
              <br><br> There are 1,386,850 workers with a <b>doctoral degree</b>.<p>")

text8 <- HTML("<H2> In Sum </H2>
              <br> <p>All things considered, the nominal median income of an average US worker is <b>$31,786</b>.
              <br>
              <br> 47% of jobs are expected to face a high risk of automatization in the near future.<sup>1</sup><p>
              <br><br><br>
              <span style='font-size:11px'><sup>1</sup><a href='https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf' target='_blank'>Frey and Osborne (2013)</a>
               write that 'associated occupations are potentially automatable over
              some unspecified number of years, <i>perhaps a decade or two.'</i></span>")

text9 <- HTML("<H2>Dashboard View</H2>
              <br> <p>The dashboard view gives a comprehensive view of all the data for easy to use and specific <p>")
text10 <- HTML("<H2>Dashboard View</H2>")

plot__pfa_map_pie <- function(df_pfa, bounds_pfa, year_range, pfa_select) {
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  
  
  df <- df_pfa %>%
    filter(selfDefinedEthnicityGroup!='Not Stated / Unknown') %>%
    group_by(pfaName, selfDefinedEthnicityGroup) %>%
    mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
    #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
    ungroup() %>% 
    distinct(pfaName, selfDefinedEthnicityGroup, .keep_all=T) %>%
    select(pfaName, selfDefinedEthnicityGroup, numberOfSearches) %>%
    pivot_wider(id_cols='pfaName', names_from='selfDefinedEthnicityGroup', values_from='numberOfSearches') 
  names(df)[-1] <- paste0("g", 1:5)
  df$null <- 1
  df <-  df %>% rowwise() %>% mutate(total = sum(c_across(g1:g4)))
  
  df$totalz <- (df$total - min(df$total))/(sort(df$total, TRUE)[3]-min(df$total)) * (55 - 13) + 13
  df <- df %>% mutate(totalz = case_when(pfaName=="West Midlands"~60, 
                                         pfaName=='Metropolitan Police'~65, T~totalz))
  df <- df %>% filter(pfaName != 'London, City of') %>% arrange(-totalz)
  df$ntile <- ntile(df$total, 12)
  
  min <- min(df$total)
  max <- sort(df$total, TRUE)[1]+sort(df$total, TRUE)[1]
  
  #df <- df %>% mutate(total = case_when(pfaName=='Metropolitan Police'~NA_real_, T~total))
  
  #df$name <- paste0('name', 1:43)
  stops <- data.frame(
    c=seq(.1,1, 0.1),
    q=rev(paste0(c('#200000', '#4a0000', '#710000', '#9b0000', '#c70000', '#e73131', '#ed6868', '#f29191', '#f6b7b7', '#fadada'),'99'))
  )
  ##4a0000, #c70000 ##f29191 #fadada
  stops <- list_parse2(stops)
  
  
  
  df$ntile2 <- (df$ntile*1.15)+5
  df$ntile3 <- df$ntile2*1.8
  
  df <- df %>% select(-c(null,total, totalz))
  
  df <- df %>%
    mutate(
      max = pmax.int(g1, g2, g3, g4),
      type = case_when(
        max == g1 ~ 1,
        max == g2 ~ 2,
        max == g3 ~ 3,
        TRUE     ~ 4)
    )
  
  
  
  #browser()
  
  df_t <- df %>% group_by(pfaName) %>% rowwise() %>% summarise(total = sum(c_across(g1:g4))) %>% ungroup()
  df_t <- merge(df_t, bounds_pfa__df[1:3], by.x='pfaName', by.y='pfa16nm')
  df_tg <- data.frame(name=df_t$pfaName, lat = df_t$lat, lon = df_t$long, z=c(runif(42, 10000 , 1000000)))
  
  
  
  highchart() %>%
    hc_add_series_map(
      id='map',
      bounds_pfa, df, value='type', joinBy=c("pfa16nm", "pfaName"),
      keys =c('pfaName', 'g1', 'g2', 'g3', 'g4', 'g5', 'null', 'ntile', 'ntile2', 'ntile3', 'max', 'type', 'total'),
      borderColor='#FAFAFA', borderWidth=.1, 
      dataLabels = list(enabled = F, format = "{point.g1}",
                        style=list(fontSize='10'))
    )%>%
    hc_add_series(data=df_tg, type= 'mapbubble',
                  color= 'blue', maxSize='0.0001%') %>%
    hc_mapNavigation(
      enabled = T, enableMouseWheelZoom = T, enableDoubleClickZoom = F,
      style=list(
        fill='white', color='white'
      ),
      
      buttonOptions = list(
        align='right', x=-110,
        theme=list(
          fill='white', `stroke-width`= 3,
          stroke='#f0f2f4', r=10,
          states=list(
            hover=list(
              fill = '#ced1d6'
            )
          )
        )
        
        
      )
      
    ) %>%
    
    
    hc_colorAxis(
      dataClasses=list(
        list(
          name='Asian',
          color = '#f29191',
          from=1, 
          to=1
        ),
        list(
          name='Black',
          color = '#e73131',
          from=2,
          to=2
        ),
        list(
          name='Other',
          color = '#9b0000',
          from=3,
          to=3
        ),
        list(
          name='Mixed',
          color = '#4a0000', 
          from=4,
          to=4
        )
      )
    ) %>%
    # stops=stops
    # dataClasses=list(
    #   list(
    #     name='Asian',
    #     color='#a0dfb9'
    #   ),
    #   list(
    #     name='Black',
    #     color='#7fabaa'
    #   ),
    #   list(
  #     name='Mixed',
  #     color='Mixed'
  #   ),
  #   
  #   list(
  #     name='White',
  #     color='#Other'
  #   )
  # 
  #   
  # )#5f7a9c
  #) %>%
  # hc_tooltip(
  #  # hideDelay=0
  # ) %>%
  hc_plotOptions(
    map = list(
      enableMouseTracking = T
    ),
    pie = list(
      point=list(
        stickyTracking=F,
        allowPointSelect = T,
        states = list(
          hover = list(
            enabled=T,
            size=80
          )
          
        )
      )
    )
    #     
    #     events=list(
    #     mouseOver = JS("function() { 
    #                    this.series.data.forEach(function(point) {
    #                    if(point.size !== point.ntile3){ point.update({size: point.ntile3})}}}"),
    #     mouseOut = JS("function() { 
    #                    this.series.data.forEach(function(point) {
    #                    if(point.size === point.ntile3){ point.update({size: point.ntile2})}}}")
    #   )
    #   )
    # )
    
    
    
    #                    if(this.options.size !== this.ntile3) {this.update({size: this.ntile3})} }"),
    #     mouseOut = JS("function() { if(this.options.size === this.ntile3) {this.update({size: this.ntile2})} }")
    #   )
    # #)
    #  events = list(
    #    mouseOver = JS("function(){this.series.data.forEach(function(point) { if(point.size != )
    #                   
    #                   if(this.options.size !== this.ntile3){ this.update({size: this.ntile3})} } "),
    #    mouseLeave =  JS("function(){ if(this.options.size === this.ntile3){ this.update({size: 80})} } ")
    # # )
    #        mouseOver = JS("function() { if(this.options.size !== 50) {this.update({size: 50})} }"),
    #        mouseOut = JS("function() { if(this.options.size === 50) {this.update({size: 20})} }")
    #TODO Chnage pie size on hover
    #https://stackoverflow.com/questions/52066731/highcharter-change-highlight-color-on-hover
    #https://jsfiddle.net/r890aact/1/
    # )
    #)#,
    #https://stackoverflow.com/questions/55884521/alternative-way-to-change-hovered-series-and-its-points-properties-in-a-highchar
    # states = list(
    #   hover = list(
    #     enabled = F#,
    #     #lineWidth = 10
    #   )
    #),
    #)
  ) %>%
    hc_legend(backgroundColor='#FFFFFF',
              bubbleLegend=list(enabled=T,
                                borderWidth= .4,
                                borderColor='#000000',
                                color= '#ffffff',
                                connectorColor= '#000000',
                                align = "right",
                                maxSize=35,
                                ranges=list(
                                  list(value=1000),
                                  list(value=10000),
                                  list(value=50000),
                                  list(value=500000),
                                  list(value=1000000)
                                ),
                                labels=list(align='right')))%>%
    # hc_legend(enabled = TRUE, layout = "vertical", 
    #           align = "right", bubbleLegend =  list(enabled = TRUE)) %>%
    hc_add_theme(hc_theme(chart = list(backgroundColor = '#FFFFFF'))) %>%
    
    #   minColor = "#fadada",
    #   maxColor = "#e10000",
    #   min=min,
    #   max=max
    # )%>%
    # hc_add_series(mapData = bounds_pfa, showInLegend = FALSE, nullColor='#f7f7f7') %>%
    hc_add_dependency("modules/series-on-point.js")%>%
    # highcharter::hc_colorAxis(
    # minColor='#f7f7f7', maxColor='#f7f7f7'#,
    #   dataClasses=list(
    #     list(
    #       color = '#551e4F',
    #       id='g4',
    #       name = 'Asian'
    #     ),
    #     list(
    #       color = '#F06043',
    #       id='g2',
  #       name = 'Black'
  #     ),
  #     
  #     list(
  #       color = '#B41658',
  #       id='g3',
  #       name = 'Mixed'
  #     ),
  #     list(
  #       color = '#F6C09E',
  #       id='g1',
  #       name = 'White'
  #     ),
  #     list(
  #       #color='#f7f7f7',
  #       id='null',
  #       name=""
  #     )
  #   )
  # ) %>%
  hc_chart(
    events = list(
      load = highcharter::JS(
        "function() {
           var chart = this;
           chart.series[0].points.forEach(pfa => {
             if (!pfa.isNull) {
               chart.addSeries({
                 type: 'pie',
                 //innerSize: '67%',
                 //borderColor: 'black',
                 borderWidth: .4,
                 borderRadius: 8,
                 borderColor:'#000000',
                 name: pfa.pfaName,
                 zIndex: 6,
                 size: pfa.ntile2,
                 dataLabels: {
                   enabled: false
                 },
                 onPoint: {
                   id: pfa.id
                 },
                 states: {
                   inactive: {
                     enabled: false
                   }
                 },
                 data: [
                   {
                     name: 'Asian',
                     y: pfa.g1,
                     color: '#f29191',
                     selected: true
                   },
                   {
                     name: 'Black',
                     y: pfa.g2,
                     color: '#e73131',
                     selected: true
                   },
                   {
                     name: 'Other',
                     y: pfa.g3,
                     color: '#9b0000',
                     selected: true
                   },
                   {
                     name: 'Mixed',
                     y: pfa.g4,
                     color: '#4a0000',
                     selected: true
                   }//,
                   //{
                     //name: 'g5',
                     //y: pfa.g5,
                     //color: '#f7f71a'
                   //}
                 ]
               },false
               );
             }
          });
          chart.redraw();
        }"
      )
    )
  ) 
  
  
  
}