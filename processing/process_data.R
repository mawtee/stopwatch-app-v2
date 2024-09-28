source('processing/processing__funs.R')
YEARS <- c(2020, 2021, 2022)
COLNAMES__LIST <- list(
  c(
    'financialYear','pfaCode','pfaName','region','legislation','reasonForSearch', 
    'selfDefinedEthnicGroup', 'selfDefinedEthnicity','numberOfSearches', 'numberOfArrests'
  ),
  c(
    'financialYear','pfaCode','pfaName', 'region', 'legislation', 'reasonForSearch', 
    'outcome', 'selfDefinedEthnicity', 'selfDefinedEthnicGroup', 'numberOfSearches'
  ),
  c(
    'financialYear','pfaCode','pfaName', 'region', 'legislation', 'reasonForSearch', 
    'outcome', 'selfDefinedEthnicity', 'selfDefinedEthnicGroup', 'numberOfSearches'
  )
)

# A list of years (inset additional year for new data)
# At the time of writing, the ONS does not publish yearly population estimates by ethnicity and so the next publication year is assumed to be 2031
YEARS_CENSUS <- c(2011, 2021)

# A list of numbers denoting the number of rows to skip  when loading census data (insert additional input if additional year)
NSKIP_CENSUS <- c(8,0)  

# A list of column name lists (create additional list and add to list of lists (COLNAMES_CENSUS) if additional year)   
COLNAMES0_CENSUS <- c('laName', 'laCode', paste0('v_', 1:18))
COLNAMES1_CENSUS = c('laCode', 'laName', 'ethnicGroupCode', 'ethnicGroupName', 'count')
COLNAMES_CENSUS = list(COLNAMES0_CENSUS, COLNAMES1_CENSUS)

################################################################################

# Stop and search
df_ss__list <- lapply(
  1:length(YEARS), function(y) {
    df <- process_stop_and_search_dashboard(YEARS[y], COLNAMES__LIST[[y]])
    return(df)
  }
)
df_ss <- bind_rows(df_ss__list) 


# Census
df_census__list <- lapply(
  1:length(YEARS_CENSUS), function(y) {
    dfCenusLA <- load_and_format_census(YEARS_CENSUS[y], NSKIP_CENSUS[y], COLNAMES_CENSUS[[y]])
    dfCensusPFA <- aggregate_census_LA_to_PFA(dfCenusLA, YEARS_CENSUS[y])
  }
)
df_census <- bind_rows(df_census__list) %>% expand_census()

df_processed <- left_join(
  df_ss, df_census, by=c('pfaCode', 'year', 'selfDefinedEthnicity')
)

# QA
expectednas <- sum(df_ss$selfDefinedEthnicGroup=='Not Stated / Unknown') + sum(df_ss$pfaName=='British Transport Police')
complete <- nrow(df_processed[complete.cases(df_processed), ])
max(c(complete,nrow(df_ss)-expectednas)) - min(c(complete,nrow(df_ss)-expectednas))
complete==nrow(df_ss)-expectednas


df_processed <- df_processed[complete.cases(df_processed), ] %>%
  select(-selfDefinedEthnicGroup.x) %>%
  rename('selfDefinedEthnicGroup' = 'selfDefinedEthnicGroup.y')



write_csv(df_processed, "data/dfPFA_clean_dashboard_pop.csv")









#%>% left_join(crime_df, by=c('pfaName', 'financialYear'))


# add crime data


write_csv(df_process, "data/dfPFA_clean_dashboard_nopop.csv")

##################################################################








#   
#  t <- df_pfa %>%
#   #filter(reasonForSearch!='Prevent acts of terrorism') %>%
#   # mutate(reasonForSearch = case_when(
#   #   reasonForSearch=='Terrorism Act 2000 s.43a'~'Terrorism',
#   #   reasonForSearch=='Prevent acts of terrorism'~'Terrorism',
#   #   reasonForSearch=='Stolen Property'~'Burglary, Robbery and Theft',
#   #   reasonForSearch=='Going Equipped'~'Burglary, Robbery and Theft',
#   #   reasonForSearch=='Other'~'Other discrete reasons (such as fireworks)',
#   #   reasonForSearch=='Offensive Weapons'~'Offensive Weapons and Firearms',
#   #   reasonForSearch=='Firearms'~'Offensive Weapons and Firearms',
#   #   reasonForSearch=='Anticipation of violence'~'Anticipation of Violence',
#   #   T~reasonForSearch
#   # )) %>%
#    # mutate(reasonForSearch = case_when(
#    #   reasonForSearch=='Anticipation of violence'~'Anticipation of Violence',
#    #   T~reasonForSearch
#    # )) %>%
#   group_by(outcome, reasonForSearch) %>%
#   summarise(numberOfSearches_N = sum(numberOfSearches, na.rm=T)) %>%
#   pivot_wider(names_from='outcome', values_from='numberOfSearches_N') %>%
#   ungroup() %>%
#   mutate(arrestRate = Arrest/(Arrest+`No Arrest`)*100) %>%
#   mutate(numberOfSearches = Arrest + `No Arrest`) %>%
#   #arrange(totsearch) %>%
#   mutate(totalSearches = sum(numberOfSearches)) %>%
#   mutate(propOfSearches = (numberOfSearches/totalSearches)*100) %>%
#   #arrange(-arrestRate) %>%
#   mutate(reasonForSearch = factor(reasonForSearch, ordered=T)) %>%
#   mutate(reasonForSearch = fct_reorder(factor(reasonForSearch),propOfSearches)) %>%
#   arrange(-match(reasonForSearch, levels(reasonForSearch)))
# 
# 
# 
# # double bar chart
# # drugs most common reason for search
# # 
# # t$reasonForSearch <- factor(
# #   
# #   
# #   t$reasonForSearch, ordered=T,levels=c('Other','Terrorism Act 2000 s.43a', 'Stolen Property', 'Anticipation of violence',
# #                               'Going Equipped', 'Offensive Weapons', 'Drugs','Prevent acts of terrorism',
# #                               'Anticipation of Violence','Firearms','Criminal Damage'
# #                               ))
#  
#  # levels <-c(
#  #   'Drugs', 'Stolen Property', 'Offensive Weapons','Going Equipped',
#  #    'Other', 'Anticipation of Violence', 'Criminal Damage',
#  #    'Firearms','Terrorism Act 2000 s.43a', 'Prevent acts of terrorism'
#  # ) 
#  # t$reasonForSearch <- factor(t$reasonForSearch, levels)
#  # t <- t[order(t$reasonForSearch), ]
#  # t$reasonForSearch <- factor(
#  #   t$reasonForSearch, ordered=T,
#  #   levels=c('Drugs', 'Stolen Property', 'Offensive Weapons','Going Equipped',
#  #            'Other', 'Anticipation of Violence', 'Criminal Damage',
#  #            'Firearms','Terrorism Act 2000 s.43a', 'Prevent acts of terrorism'
#  #   )
#  # )
#  #            
# 
#  
# 
# 
# 
# 
# plot_left <- highchart() %>%
#   hc_xAxis(categories = factor(t$reasonForSearch), opposite=T) %>%#
#   # hc_xAxis(categories = list(
#   #   'Other','Prevent acts of terrorism', 'Firearms',
#   #   'Stolen Property','Offensive Weapons', 'Criminal Damage',
#   #   'Terrorism Act 2000 s.43a','Drugs','Going Equipped',
#   #   'Anticipation of Violence','Anticipation of violence'
#   # ),opposite=T) %>%
#   hc_add_series(name='% of searches',type='bar', data=t$searchp, color='#cacaca') %>%
#  
#   hc_yAxis(reversed=T) 
# 
# plot_right <- highchart() %>%
#   hc_xAxis(categories = t$reasonForSearch, labels=list(enabled=F)) %>%
#   hc_add_series(name='Arrest Rate', type='bar', data=t$arrestRate, color='#e10000') %>%
#   hc_yAxis(max=60)
# 
#  combineWidgets(plot_left, plot_right, nrow=1, colsize=c(1.4,1))
#  
#  
#  
#  # The more specific the reason, the higher the rate of arrest 
#  
#  
#  # https://cran.r-project.org/web/packages/manipulateWidget/vignettes/manipulateWidgets.html
#  
#  
#  # searches per 10,000
#  # crimes per 10,000
#  
#  # Crime susceptible to stop and search
#  # Total
#  # Drugs
#  
#  
#  # For single years, have a reference year and show change in searches from ref versys change in crime from ref
#  # Number of searches gone up, but crime has stayed the same
# ###############################################################################
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
#  ###############################################################################
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
# t %>%
#   hchart(type='bar', hcaes(x=reasonForSearch, y=arrestRate)) %>%
#   hc_xAxis(xAxis=1)
# # Searches conducted on the grounds of 'Terrorism' and 'Other'  had the highest arrest rates at 40%. Yet these searches made up only x
# # Searched conudcted on the grounds of 'Drugs' had one the lowest arrest rates - these searches make up for more thasn 50% of all searches
# 
# 
# 
# 
# 
# df <- data.frame(y=c(123,89,106))
# 
# highchart() %>% 
#   hc_chart(type = "line", polar = TRUE) %>% 
#   hc_xAxis(categories = c("Category 1", "Category 2", "Category 3"),
#            labels = list(style = list(whiteSpace = 'nowrap'))) %>%
#   hc_yAxis(max = plyr::round_any(max(df$y), 10, ceiling),
#            plotLines = list(list(value = 100,
#                                  color = 'orange',
#                                  width = 3)),
#            gridLineColor = 'green') %>%
#   hc_series(
#     list(name = "Example chart",
#          data = df$y,
#          pointPlacement = "on",
#          type = "line",
#          color = "blue",
#          showInLegend = F)) 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# %>%
#   group_by(year, outcome) %>%
#   mutate(outcome_N = sum(numberOfSearches, na.rm=T)) %>%
#   group_by(year) %>%
#   mutate(numberOfArrests = min(outcome_N)) 
# 
# 
# %>%
#   summarise(propArrest = (numberOfArrests/numberOfSearches)*100) %>%
#   ungroup() %>%
#   distinct(year, .keep_all=T)
# 
# %>%
# 
# 
# %>%
#  
#   summarise(propArrest = (numberOfArrests/numberOfSearches)*100) %>%
#   ungroup() %>%
#   distinct(year, .keep_all=T)
# 
#   mutate(arrest = case_when(outcome == "Arrest"~1, T~0)) %>%
#   group_by(year) %>%
#   mutate(numberArrest = sum(arrest)) %>%
#  