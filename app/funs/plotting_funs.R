colour_list <- c()


# National Trends timeline plot
#===============================================================================
plot__nattrend_timeline <- function(df_pfa, year_range, browser_width, browser_height) {
  
  # INPUTS AND SHIT
  #----------------------
  
  
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
  #---------------------------
  
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
  
  # Plot timeline
  #--------------------------------------------
  plot <- highchart() %>%
    hc_add_dependency("modules/timeline.js") %>%
    hc_chart(type="timeline") %>%
    hc_xAxis(visible=F) %>%
    hc_yAxis(visible=F) %>%
    hc_title(text="Timeline of space exploration") %>%
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
    hc_plotOptions(series=list(
      animation=list(duration=timeline_anim_duration),
      lineWidth=1))
 
  
  return(plot)
}
  
  
  
  
  





















plot__pfa_scr_nattrend_agg <- function(df_pfa, year_range, group) {
  
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  #browser()
  
  if (group=="year") {
    df_pfa <- df_pfa %>%
      group_by(year) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
     # mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
      ungroup() %>% 
      distinct(year, .keep_all=T) %>%
      select(year, .data[[group]], numberOfSearches,)
  }
  if (group!="year") {
    df_pfa <- df_pfa %>%
      group_by(year, .data[[group]]) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
      ungroup() %>% 
      distinct(year, .data[[group]], .keep_all=T) %>%
      select(year, .data[[group]], numberOfSearches)
  }


  plot <- df_pfa %>%
    hchart(
      'column', hcaes(x=year, y=numberOfSearches, group=.data[[group]])
    ) %>%
    hc_plotOptions(
      series = list(stacking = "normal")
    ) %>%
    hc_yAxis(
      reversedStacks=F
    )

  return(plot)

}

















  



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
