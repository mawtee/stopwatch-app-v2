colour_list <- c()

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
