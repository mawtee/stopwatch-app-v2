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


load_and_format_census <- function(year, nskip, colnames) {
  
  # """
  #  Description: 
  #     Loads, cleans and aggregates Local Authority (LA) ethnnic group census population counts 
  #     to the level of Police Force Area (PFA).
  #    
  #  Inputs:
  #     'year' - The year (or a list of years) corresponding to the publication year 
  #     of the census dataset(s) [download from ]
  #     'nskip' - A list of numbers denoting the number of rows to skip when loading data
  #     'colnames' - A list of column name lists giving the name of the columns for each dataframe
  #     
  #  Returns:
  #     'dfCensusLA' - a formatted dataframe for ethnicity census data at LA level and
  #     corresponding to the input year
  #  """
  
  # QA params
  # year <- YEARS_CENSUS[2]
  # nskip <- NSKIP_CENSUS[2]
  # colnames <- COLNAMES_CENSUS[[2]]
  
  #; Load census data
  print(paste0("Loading and aggregating ",year," census data"))
  dfCensusLA <- read_csv(paste0('data/',year,'_census_la.csv'), skip=nskip)
  
  #; Rename columns
  names(dfCensusLA) <- colnames
  
  #; Reshape 2011 ethnicity columns from wide to long (and de-string ethnic group values)
  if (year == 2011) {
    dfCensusLA <- dfCensusLA %>% 
      pivot_longer(-c('laCode', 'laName'), names_to='ethnicGroupCode', values_to='count') %>%
      mutate(ethnicGroupCode = as.numeric(as.character(str_remove(ethnicGroupCode, 'v_')))) %>%
      filter(!is.na(count))
  }
  # Drop -8 values ('Does not apply') from 2021 data
  if (year == 2021) {
    dfCensusLA <- dfCensusLA %>% filter(ethnicGroupCode != -8)
  }
  
  #  Add full ethnicity names
  if (year == 2011) {
    original_grouping <- c(1:18)
    new_grouping <- c(
      'Indian or British Indian',
      'Pakistani or British Pakistani',
      'Bangladeshi or British Bangladeshi',
      'Chinese',
      'Other Asian or Asian British',
      'Black or Black British African',
      'Black or Black British Caribbean',
      'Other Black or Black British',
      'Mixed White and Black Caribbean',
      'Mixed White and Black African',
      'Mixed White and Asian',
      'Other Mixed',
      'White British',
      'White Irish',
      'Gypsy or Irish Traveller',
      'Other White',
      'Arab',
      'Other'
    )
  }
  if (year == 2021) {
    original_grouping <- c(1:19)
    new_grouping <- c(
      'Bangladeshi or British Bangladeshi',
      'Chinese',
      'Indian or British Indian',
      'Pakistani or British Pakistani',
      'Other Asian or Asian British',
      'Black or Black British African',
      'Black or Black British Caribbean',
      'Other Black or Black British',
      'Mixed White and Asian',
      'Mixed White and Black African',
      'Mixed White and Black Caribbean',
      'Other Mixed',
      'White British',
      'White Irish',
      'Gypsy or Irish Traveller',
      'Other White',
      'Other White',
      'Arab',
      'Other'
    )
  }
  dfCensusLA$selfDefinedEthnicity <- recode(
    dfCensusLA$ethnicGroupCode, !!! setNames(new_grouping,original_grouping), .default=NA_character_)
  
  # # Add some confirmation/QA logic
  # nrow(dfCensusLA)
  # t <- dfCensusLA[complete.cases(dfCensusLA), ]
  # nrow(t)
  
  
  #; Create broad ethnic groupings
  #...one more white category in 2021 data, hence adjustment down to 2011 codings to match stop-search data
  ethnicGroups <- c("Asian", "Black", "Mixed", "White", "Other Ethnic Group")
  asianGroup <- 1:5
  blackGroup <- 6:8
  mixedGroup <- 9:12
  if (year == 2011) {
    whiteGroup <- 13:16 
    otherGroup <- 17:18
  }
  if (year == 2021) {
    whiteGroup <- 13:17
    otherGroup <- 18:19 
  }
  dfCensusLA <- dfCensusLA %>%
    mutate(selfDefinedEthnicGroup = case_when(
      ethnicGroupCode %in% asianGroup~'Asian',
      ethnicGroupCode %in% blackGroup~'Black',
      ethnicGroupCode %in% mixedGroup~'Mixed',
      ethnicGroupCode %in% whiteGroup~'White',
      ethnicGroupCode %in% otherGroup~'Other Ethnic Group',
      T~NA_character_
    )) 
  
  return(dfCensusLA)
  
}




aggregate_census_LA_to_PFA <- function(dfCensusLA, year) {
  
  #"""
  # Description: 
  #    Aggregates census data from LA to PFA level using LA-PFA lookup table.
  #   
  # Inputs:
  #    'dfCensusLA' - A formatted dataframe for LA census data correspdoning
  #    to the input year
  #    'year' - The year (or a list of years) corresponding to the publication year 
  #    of the census dataset(s)
  # Returns:
#    'dfCensusPFA' - a formatted dataframe for ethnicity census data at PFA level and
#    corresponding to the input year
# """

#; Lookup 2021 LA codes for 2011 LA census data via merge
if (year == 2011) {
  dfCensusLA <- dfCensusLA %>%
    mutate(laCode = case_when(
      laCode=='E07000100'~'E07000240',
      laCode=='E07000104'~'E07000241',
      laCode=='E07000097'~'E07000242',
      laCode=='E07000101'~'E07000243',
      laCode=='E06000048'~'E06000057',
      laCode=='E08000020'~'E08000037',
      laCode %in% c('E07000205', 'E07000206')~'E07000244',
      laCode %in% c('E07000201', 'E07000204')~'E07000245',
      laCode %in% c('E07000190', 'E07000191')~'E07000246',
      laCode %in% c('E06000028', 'E06000029', 'E07000048')~'E06000058',
      laCode %in% c('E07000049', 'E07000050', 'E07000052', 'E07000051', 'E07000053')~'E06000059',
      laCode %in% c('E07000004', 'E07000005', 'E07000006', 'E07000007')~'E06000060',
      laCode %in% c('E07000150', 'E07000152', 'E07000153', 'E07000156')~'E06000061',
      laCode %in% c('E07000151', 'E07000154', 'E07000155')~'E06000062', 
      T~laCode
    )) %>%
    group_by(laCode, selfDefinedEthnicity) %>%
    mutate(count = sum(count))
}

#; Check number of LAs (should be 331 for all years)
print(paste0("Total number of LAs in ", year, " data = ", n_distinct(dfCensusLA$laCode)))

#; Load LA to PFA lookup
dfLookupPFA = read_csv('data/la_pfa_lookup.csv')

# Keep distinct vals of LA
dfLookupPFA <- dfLookupPFA %>% 
  distinct(LAD21CD, .keep_all=T)

#; Drop redundant columns and rename
dfLookupPFA <- dfLookupPFA %>%
  select('laCode'='LAD21CD',
         'pfaCode'='PFA21CD',
         'pfaName'='PFA21NM')

#; Merge with census data
dfCensusPFA <- left_join(dfCensusLA, dfLookupPFA, by='laCode')

#; Aggregate (sum) population count for PFA by ethnicity
dfCensusPFA <- dfCensusPFA %>%
  group_by(pfaCode, pfaName, selfDefinedEthnicity) %>%
  mutate(count = sum(count)) %>%
  ungroup() %>%
  rename('population'='count')


#; Check number of PFAs (43)
print(paste0("Total number of PFAs in " ,year, " data = ",  n_distinct(dfCensusPFA$pfaCode)))

#; Year indicator
dfCensusPFA$year <- year

dfCensusPFA <- dfCensusPFA %>%
  select(year, pfaCode, pfaName, selfDefinedEthnicity,  population, selfDefinedEthnicGroup,)

return(dfCensusPFA)

}


expand_census <- function(dfCensusPFA) {

  # """
  #  Description:
  #     Expands LA census data into a yearly 2011-2021 time series.
  #
  #  Inputs:
  #     'dfCensusPFA' - a formatted dataframe for ethnicity census data at
  #     PFA level andcorresponding to the input year
  #  Returns:
  #     'dfCensusPFAseries' - a formatted timeseries dataframe for ethnicity census data
  #     at PFA level
  #  """
  
  # QA check
  #t2 <- t %>% filter(pfaCode=='E23000001' & selfDefinedEthnicity=='Arab')

  # Expand dataframe 6 times (6*2) to create observations required for yearly time series 
  dfCensusPFAseries <- dfCensusPFA %>%
    group_by(year, pfaCode, selfDefinedEthnicity, population, selfDefinedEthnicGroup) %>%
    expand(id = 1:6) %>%
    arrange(id) 
  
  # Recode population variable
  dfCensusPFAseries <- dfCensusPFAseries %>%
    arrange(pfaCode, year, selfDefinedEthnicity) %>%
    group_by(pfaCode, selfDefinedEthnicity) %>%
    mutate(cumCount = row_number()) %>%
    mutate(yearSeries=plyr::mapvalues(cumCount,from=1:12,to=2011:2022)) %>%
    mutate(pop11 = first(population)) %>%
    mutate(pop21 = last(population)) %>%
    mutate(population = case_when(yearSeries < 2021~pop11, T~pop21)) %>%
    filter(cumCount != 12) %>%
    mutate(year = as.character(yearSeries)) %>%
    ungroup() %>%
    select(year:selfDefinedEthnicGroup)
    
  return(dfCensusPFAseries)



}











