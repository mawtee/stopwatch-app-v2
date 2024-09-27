process_stop_and_search <- function(year, colnames) {
  

  # Load data
  print(paste0("Loading and formatting ", year, " stop and search data"))
  if (year==2020) {
    print("Note that 2020 data refers to the historical time series from 2011/12-2019/20")
  }
  df <- read_csv(paste0("data/ss_open_",year,".csv"))
  
  # Drop redundant columns 
  if (year==2020) {
    todrop <- names(df)[c(7, (length(names(df))-2):length(names(df)))]
    df <- df %>% select(-all_of(todrop))
  }
  if (year!= 2020) {
    todrop <- c('financial_year_quarter', 'link', 'gender', 'sex', 'age_group')
    df <- df %>% select(-any_of(todrop))
  }
  
  # Rename columns
  names(df) <- colnames
  
  # Homogenise Police Force Area names
  df <- df %>%
    mutate(pfaName = case_when(
      pfaName=='Avon and Somerset'~'Avon & Somerset',
      pfaName=='Devon and Cornwall'~'Devon & Cornwall', T~pfaName
    ))
  
  # Homogenise (and recode) ethnicity group categories
  original_grouping <- c(
    'Black or Black British',
    'Asian or Asian British',
    'N/A - vehicle search',
    'Vehicle only',
    'Not stated',
    'Not Stated'
  )
  new_grouping <- c(
    'Black', 
    'Asian',
    'Not Stated / Unknown', 
    'Not Stated / Unknown', 
    'Not Stated / Unknown',
    'Not Stated / Unknown'
  )
  df$selfDefinedEthnicGroup <- recode(
    df$selfDefinedEthnicGroup, !!! setNames(new_grouping,original_grouping), default=df$selfDefinedEthnicGroup)
  # Reshape historical data to match format of later releases
  if (year == 2020) {
    # Transform to numeric
    df <- df %>% 
      mutate(across(c(numberOfSearches, numberOfArrests), ~ as.numeric(as.character(stringr::str_trim(.x))))) %>%
      mutate(across(c(numberOfSearches, numberOfArrests), ~ case_when(is.na(.x)~0, T~.x)))
    # Create a copy of existing dataframe, keeping only Arrests
    df_arrests <- df %>%
      mutate(numberOfSearches = numberOfArrests) %>%
      filter(numberOfSearches > 0) %>%
      mutate(outcome = 'Arrest') %>%
      select(-numberOfArrests)

    # On main dataframe, subtract number of arrest from number of searches and then append on the arrests dataframe

    df <- df %>%
      #mutate(id = row_number()) 
      #4002
      mutate(numberOfArrests = case_when(numberOfArrests >=.75*numberOfSearches~numberOfSearches*.3, T~numberOfArrests)) %>%
      mutate(numberOfSearches = numberOfSearches - numberOfArrests) %>%
      mutate(outcome = 'No Arrest') %>%
      select(-numberOfArrests) %>%
      bind_rows(df_arrests) %>%
      mutate(year =  sprintf("%.4s", financialYear)) %>%
      filter(year >=2011) %>%
      select(financialYear, year, pfaCode:reasonForSearch,  selfDefinedEthnicGroup, selfDefinedEthnicity, outcome, numberOfSearches)
  }
  
  # Reshape recent releases to match historical level of aggregration (effectively, removing split at level of age group)
  if (year != 2020) {
    df <- df %>%
      mutate(outcome = case_when(outcome!='Arrest'~'No Arrest', T~'Arrest')) %>%
      group_by(financialYear, pfaName, legislation, reasonForSearch, selfDefinedEthnicity, selfDefinedEthnicGroup, outcome) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      ungroup() %>%
      distinct(financialYear, pfaName, legislation, reasonForSearch, selfDefinedEthnicity, selfDefinedEthnicGroup, outcome, .keep_all=T) %>%
      mutate(year =  sprintf("%.4s", financialYear)) %>%
      select(financialYear, year, pfaCode:reasonForSearch, selfDefinedEthnicGroup,selfDefinedEthnicity, outcome, numberOfSearches)
  }
  
  # Recode reason for search
  df <- df %>%
  mutate(reasonForSearch = case_when(
    reasonForSearch=='Terrorism Act 2000 s.43a'~'Terrorism',
    reasonForSearch=='Prevent acts of terrorism'~'Terrorism',
    reasonForSearch=='Stolen Property'~'Burglary, Robbery and Theft',
    reasonForSearch=='Going Equipped'~'Burglary, Robbery and Theft',
    reasonForSearch=='Other'~'Other discrete reasons (such as fireworks)',
    reasonForSearch=='Offensive Weapons'~'Offensive Weapons and Firearms',
    reasonForSearch=='Firearms'~'Offensive Weapons and Firearms',
    reasonForSearch=='Anticipation of violence'~'Anticipation of Violence',
    T~reasonForSearch
  ))

  return(df)
}




process_stop_and_search_dashboard <- function(year, colnames) {
  
  
  year <- 2020
  colnames <- COLNAMES__LIST[[1]]
  
  # Load data
  print(paste0("Loading and formatting ", year, " stop and search data"))
  if (year==2020) {
    print("Note that 2020 data refers to the historical time series from 2011/12-2019/20")
  }
  df <- read_csv(paste0("data/ss_open_",year,".csv"))
  
  # Drop redundant columns 
  if (year==2020) {
    todrop <- names(df)[c(7, (length(names(df))-2):length(names(df)))]
    df <- df %>% select(-all_of(todrop))
  }
  if (year!= 2020) {
    todrop <- c('financial_year_quarter', 'link', 'gender', 'sex', 'age_group')
    df <- df %>% select(-any_of(todrop))
  }
  
  # Rename columns
  names(df) <- colnames
  
  # Homogenise Police Force Area names
  df <- df %>%
    mutate(pfaName = case_when(
      pfaName=='Avon and Somerset'~'Avon & Somerset',
      pfaName=='Devon and Cornwall'~'Devon & Cornwall', T~pfaName
    ))
  
  # Homogenise (and recode) ethnicity group categories
  original_grouping <- c(
    'Black or Black British',
    'Asian or Asian British',
    'N/A - vehicle search',
    'Vehicle only',
    'Not stated',
    'Not Stated'
  )
  new_grouping <- c(
    'Black', 
    'Asian',
    'Not Stated / Unknown', 
    'Not Stated / Unknown', 
    'Not Stated / Unknown',
    'Not Stated / Unknown'
  )
  df$selfDefinedEthnicGroup <- recode(
    df$selfDefinedEthnicGroup, !!! setNames(new_grouping,original_grouping), default=df$selfDefinedEthnicGroup)
  # Reshape historical data to match format of later releases
  if (year == 2020) {
    # Transform to numeric
    df <- df %>% 
      mutate(across(c(numberOfSearches, numberOfArrests), ~ as.numeric(as.character(stringr::str_trim(.x))))) %>%
      mutate(across(c(numberOfSearches, numberOfArrests), ~ case_when(is.na(.x)~0, T~.x))) #%>%
      #mutate(year =  sprintf("%.4s", financialYear))
    # Create a copy of existing dataframe, keeping only Arrests
    df_arrests <- df %>%
      mutate(numberOfSearches = numberOfArrests) %>%
      filter(numberOfSearches > 0) %>%
      mutate(outcome = 'Arrest') %>%
      select(-numberOfArrests)
    
    # On main dataframe, subtract number of arrest from number of searches and then append on the arrests dataframe
    df <- df %>%
     # mutate(numberOfArrests = case_when(numberOfArrests >=.75*numberOfSearches~numberOfSearches*.3, T~numberOfArrests)) %>%
      mutate(numberOfSearches = numberOfSearches - numberOfArrests) %>%
      mutate(outcome = 'No Arrest') %>%
      select(-numberOfArrests) %>%
      bind_rows(df_arrests) %>%
      mutate(year =  sprintf("%.4s", financialYear)) %>%
      filter(year >= 2011) %>%
      select(financialYear, year, pfaCode:reasonForSearch,  selfDefinedEthnicGroup, selfDefinedEthnicity, outcome, numberOfSearches)
  }
  
  # Reshape recent releases to match historical level of aggregration (effectively, removing split at level of age group)
  if (year != 2020) {
    df <- df %>%
      mutate(outcome = case_when(outcome!='Arrest'~'No Arrest', T~'Arrest')) %>%
      group_by(financialYear, pfaName, legislation, reasonForSearch, selfDefinedEthnicity, selfDefinedEthnicGroup, outcome) %>%
      mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
      ungroup() %>%
      distinct(financialYear, pfaName, legislation, reasonForSearch, selfDefinedEthnicity, selfDefinedEthnicGroup, outcome, .keep_all=T) %>%
      mutate(year =  sprintf("%.4s", financialYear)) %>%
      select(financialYear, year, pfaCode:reasonForSearch, selfDefinedEthnicGroup,selfDefinedEthnicity, outcome, numberOfSearches)
  }
  
  # Recode reason for search
  # df <- df %>%
  #   mutate(reasonForSearch = case_when(
  #     reasonForSearch=='Terrorism Act 2000 s.43a'~'Terrorism',
  #     reasonForSearch=='Prevent acts of terrorism'~'Terrorism',
  #     reasonForSearch=='Stolen Property'~'Burglary, Robbery and Theft',
  #     reasonForSearch=='Going Equipped'~'Burglary, Robbery and Theft',
  #     reasonForSearch=='Other'~'Other discrete reasons (such as fireworks)',
  #     reasonForSearch=='Offensive Weapons'~'Offensive Weapons and Firearms',
  #     reasonForSearch=='Firearms'~'Offensive Weapons and Firearms',
  #     reasonForSearch=='Anticipation of violence'~'Anticipation of Violence',
  #     T~reasonForSearch
  #   ))
  
  return(df)
}



