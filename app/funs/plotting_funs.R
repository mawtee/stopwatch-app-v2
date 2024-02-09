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
  
  
  
  
  





















plot__pfa_scr_nattrend_agg <- function(df_pfa, year_range, year_range_scr, group) {
  

  
 # browser()
  
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  #browser()
  df_pfa <- df_pfa %>%
    group_by(year, .data[[group]]) %>%
    mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
    #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
    ungroup() %>% 
    distinct(year, .data[[group]], .keep_all=T) %>%
    select(year, .data[[group]], numberOfSearches) 
    #arrange(.data[[group]]) %>%               # sort your dataframe
    #mutate(group = factor(.data[[group]], unique(.data[[group]]))) # reset your factor-column based on that order
  
  
  
  
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

  plot <- df_pfa %>%
    filter(year==year_range_scr) %>%
    hchart(
      'column', hcaes(x=.data[[group]], y=numberOfSearches)
    ) %>%
    #hc_plotOptions(
    #  series = list(stacking = "normal")
    #) %>%
    hc_yAxis(
      #reversedStacks=F,
      title=list(text="Number of stop-searches"),
      max=600000,
      min=0
    ) %>%
    hc_xAxis(title=list(text="")) %>%
    
    hc_colors("#e10000") %>%
    hc_legend(itemStyle=list(fontSize="1.6vh"))

  return(plot)

}




plot__pfa_map <- function(df_pfa, bounds_pfa, year_range) {

  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  #browser()
  df_pfa <- df_pfa %>%
    group_by(pfaName) %>%
    mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
    #mutate(rateOfSearches = sum(numberOfSearches, na.rm=T)/sum(population, na.rm=T)) %>%
    ungroup() %>% 
    distinct(pfaName, .keep_all=T) %>%
    select(pfaName, numberOfSearches) 
  #df_pfa_sf <- sf::st_as_sf(df_pfa_sf)
  
  
  min <- min(df_pfa$numberOfSearches)
  max <- sort(df_pfa$numberOfSearches, TRUE)[2]+(.05*max(df_pfa$numberOfSearches))
  rownames(df_pfa) <- df_pfa$pfaName
  highchart() %>%
    hc_title(text = "") %>%
    hc_subtitle(text = "") %>%
    hc_add_series_map(bounds_pfa, df_pfa, name="Number of stop-searches", value = "numberOfSearches", joinBy=c("pfa16nm", "pfaName"), borderColor="white", borderWidth=0.1) %>%
    hc_colorAxis(
      minColor = "#fce5e5",
      maxColor = "#e10000",
      min=min,
      max=max
    ) %>%
    hc_mapNavigation(enabled = TRUE)
  
  
  
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
           text8
    )
  )
}


text2 <- HTML("<H2> Stop and search by ethnic group </H2>
              <br> <p> Workers with <font color='#A00042'>no formal education credential</font> have a median income of $25,636.
              <br> On average, those occupations have a <b>90% chance</b> of job automation.
              <br><br> There are 23,765,700 workers with <font color='#A00042'>no formal education credential</font>.<p>")

text3 <- HTML("<H2> Stop and search by region </H2>
              <br> <p>Workers with <font color='#F56C42'>high school diplomas</font> have a median income of $25,636.
              <br> On average, those occupations have a <b>60% chance</b> of job automation.
              <br><br> There are 33,129,910 workers with a <font color='#F56C42'>high school diploma</font>.<p>")

text4 <- HTML("<H2> Stop and search by legislation </H2>
              <br> <p>Workers with <font color='#008640'>postsecondary nondegree awards</font> (e.g. actors) have a median income of $39,990.
              <br> On average, those occupations have a <b>52% chance</b> of job automation.
              <br><br> There are 5,904,150 workers with a <font color='#008640'>postsecondary nondegree award</font>.<p>")

text5 <- HTML("<H2> Stop and search by reason for search </H2>
              <br> <p>Workers with <font color='#3487BD'>associate's degrees</font> have a median income of $41,496.
              <br> On average, those occupations have a <b>50% chance</b> of job automation.
              <br><br> There are 1,869,840 workers with an <font color='#3487BD'>associate's degree</font>.<p>")

text6 <- HTML("<H2> Stop and search by outcome of search </H2>
              <br> <p>Workers with <font color='#C71C7E'>bachelor's degrees</font> have a median income of $59,124.
              <br> On average, those occupations have a <b>20% chance</b> of job automation.
              <br><br> There are 18,399,270 workers with a <font color='#C71C7E'>bachelor's degree</font>.<p>")

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
