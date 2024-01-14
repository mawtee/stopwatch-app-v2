

process__df_pfa_range <- function(df_pfa, year_range, legislation_select) {
  
  # INPUTS:
    # df_pfa: processed long-form dataframe (default = df_pfa)
    # year_range: inputId of year range slider (default = input$year_range)
    # legislation_select: inputId of legislation selector (default = input$legislation_select)
  # OUTPUT
    # df_pfa_range: dataframe with one row for each PFA with sum of x across year_range 
  
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range[1]:year_range[2]],]
  if (legislation_select == "All"){ df_pfa$legislation <- "All" }
  df_pfa[df_pfa$legislation == legislation_select,]
  df_pfa <- df_pfa %>% 
    group_by(pfaName, legislation) %>% 
    summarise(across(c(numberOfSearches, population),~ sum(.x, na.rm=T))) %>%
    ungroup()
  df_pfa_range <- df_pfa
  return(df_pfa_range)
  
}

process__df_pfa_ts <- function(df_pfa, year_range, legislation_select) {
  
  # INPUTS:
    # df_pfa: processed long-form dataframe (default = df_pfa)
    # year_range: inputId of year range slider (default = input$year_range)
    # legislation_select: inputId of legislation selector (default = input$legislation_select)
  # OUTPUT
    # df_pfa_ts: dataframe with one row of the sum of x of each PFA by year 
  
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  #browser()
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  if (legislation_select == "All"){ df_pfa$legislation <- "All" }
  df_pfa[df_pfa$legislation == legislation_select,]
  df_pfa <- df_pfa %>% 
    group_by(pfaName, legislation, year) %>% 
    mutate(across(c(numberOfSearches, population),~ sum(.x, na.rm=T))) %>%
    ungroup() %>%
    distinct(pfaName, legislation, year, .keep_all=T) 
  #browser()
  df_pfa_ts <- df_pfa
  return(df_pfa_ts)
  
}

#year_range <- c(levels(df_pfa$year)[1], levels(df_pfa$year)[length(levels(df_pfa$year))])
#process__df_pfa_ts(df_pfa, year_range, legislation_select)

# create line chart showing number of searches across time
# create map showing number of searches in range for each PFA




process__df_pfa_scr_natagg <- function(df_pfa, year_range) {
  
  # INPUTS:
  # df_pfa: processed long-form dataframe (default = df_pfa)
  # year_range: inputId of year range slider (default = input$year_range)
  # legislation_select: inputId of legislation selector (default = input$legislation_select)
  # OUTPUT
  # df_pfa_ts: dataframe with one row of the sum of x of each PFA by year 
  
  year_range_int <- which(levels(df_pfa$year) %in% year_range)
  #browser()
  df_pfa <- df_pfa[df_pfa$year %in% levels(df_pfa$year)[year_range_int[1]:year_range_int[2]],]
  df_pfa <- df_pfa %>% 
    group_by(year) %>% 
    mutate(across(c(numberOfSearches, population),~ sum(.x, na.rm=T))) %>%
    ungroup() %>%
    distinct(year, .keep_all=T)
  #browser()
  df_pfa_scr_natagg <- df_pfa
  return(df_pfa_scr_natagg)
  
}




