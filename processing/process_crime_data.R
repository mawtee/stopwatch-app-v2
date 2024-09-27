#'[Script]#'*process_crime_data.R*
#'[Project]#'*stopwatch-app-v2 (https://github.com/mawtee/stopwatch-app-v2.git)*
#'[Author]#'*M. Tibbles*
#'[Last Update]#'*29/07/2024*
#'[Description]#'*This scripts processes Police API crime data for use in the *
#'*Stopwatch Interactive Stop and Search tracker. The data is downloaded from the web *
#'*and then combined and summarised by year to enable compatibility with annual Police *
#' *Powers and Procedures data published by the Home Office.*
#' https://www.gov.uk/government/statistics/police-recorded-crime-open-data-tables
#'[Libraries]
library(tidyverse)
library(readODS)
#'[Global Options]
#' Nested list containing file names of crime data and the years included in each file.
#' Note. Years correspond to the sheet names in each file. Only include years for which 
#'       Home Office Stop and Search data has been published. 
FILES_YEARS__LIST <- list(
  FILE1 = list(
    NAME='prc-pfa-mar2008-mar2012-tabs.ods',YEARS='2011_12'
  ),
  FILE2 = list(
    NAME=c('prc-pfa-mar2013-mar2024-tabs.ods'), YEARS=c(
      '2012_13', '2013_14', '2014_15', '2015_16', '2016_17', '2017_18',
      '2018_19', '2019_20', '2020_21', '2021_22'
    )
  )
)
#' Vector of strings of recorded crime types that are susceptible to stop and search
#' Note. Ensure strings are formatted as titles (capitalize first letter of each word)
SUSCEPTIBLE_CRIMES <- c(
  'Drug Offences', 'Violent Crime', 'Burglary, Robbery & Theft', 'Criminal Damage'
)
#' List of string vectors providing Offence Codes for each type of susceptible crime (define above)
#' Note. Only crimes that are deemed relevant to the object of stop and search are included. 
#'       For example, under Violent Crime, the offence of 'Cruelty to and Neglect of a child'
#'       is not included, while 'Possession of Firearms' is included. See 
#'       `"data/crime data/reccrime-offence-ref__with_edits.xlsx"` for detailed descriptions.
OFFENCE_CODES__LIST <- list(
  DRUG_OFFENCES = c(
    "92A", "92B", "92C", "92D", "92E"
  ),
  VIOLENT_CRIME = c(
    "1", "1/4.1/4.2", "1/4.1/4.2/4.10", "104", "105A", "105B", 
    "10A", "10C", "10D", "2", "3", "3A", "3B", "4.1", "5", "5A", 
    "5B", "5C", "5D", "8A", "8B", "8D", "8G", "8J", "8N", "8P"
  ),
  BURGLARY_ROBBERY_THEFT = c(
    "28", "28A", "28B", "28C", "28D", "29", "30", "30A", "30B", 
    "31", "34A", "34B", "38", "39", "40", "41", "42", "44", "46",
    "47", "49", "54"
  ),
  CRIMINAL_DAMAGE = c(
    "56", "56A", "56B", "58A", "58B", "58C", "58D", "58E", "58F", 
    "58G", "58H", "58J", "59"
  )
)
#'[____________________________________________________________________________]


crime_df__list <- lapply(
  1:length(FILES_YEARS__LIST), function(file) {
    
    df_year__list <- lapply(
      1:length(FILES_YEARS__LIST[[file]]$YEARS), function(year) {
    
    df <- readODS::read_ods(
      paste0('data/crime data/', FILES_YEARS__LIST[[file]]$NAME), sheet=FILES_YEARS__LIST[[file]]$YEARS[year]) %>%
      filter(
        `Offence Code` %in% c(
          OFFENCE_CODES__LIST$DRUG_OFFENCES,
          OFFENCE_CODES__LIST$VIOLENT_CRIME,
          OFFENCE_CODES__LIST$BURGLARY_ROBBERY_THEFT,
          OFFENCE_CODES__LIST$CRIMINAL_DAMAGE
        )
      ) %>%
      mutate(`Offence Group` = case_when(
        `Offence Code` %in% OFFENCE_CODES__LIST$DRUG_OFFENCES ~ 'Drug Offences',
        `Offence Code` %in% OFFENCE_CODES__LIST$VIOLENT_CRIME ~ 'Violent Crime',
        `Offence Code` %in% OFFENCE_CODES__LIST$BURGLARY_ROBBERY_THEFT ~ 'Burglary, Robbery & Theft',
        `Offence Code` %in% OFFENCE_CODES__LIST$CRIMINAL_DAMAGE~ 'Criminal Damage'
      )) %>%
      group_by(`Force Name`, `Offence Group`) %>%
      mutate(`Number of Offences` = sum(`Number of Offences`, na.rm=T)) %>%
      ungroup() %>%
      distinct(`Force Name`, `Offence Group`, .keep_all=T) %>%
      mutate(`Force Name` = gsub(" and ", " & ", `Force Name`)) %>%
      filter(`Force Name` != 'British Transport Police') %>%
      select(
        'financialYear'=`Financial Year`,
        'pfaName'=`Force Name`,
        'offenceGroup'=`Offence Group`,
        'numberOfOffences'=`Number of Offences`
      ) 
      }
    )
    df_years <- bind_rows(df_year__list)
    return(df_years)
     
    
    
  }
)

crime_df <- bind_rows(crime_df__list) %>%
  pivot_wider(names_from='offenceGroup', values_from='numberOfOffences') %>%
  rowwise() %>%
  mutate(crimeSusceptibleToStopAndSearch = sum(c_across(`Violent Crime`:`Drug Offences`))) %>%
  rename_with(
    ~stringr::str_replace_all(.x, c(' '='', ','='', '&'='')),  
    `Violent Crime`:`Drug Offences`
  )
substr(names(crime_df), 1, 1) <- tolower(substr(names(crime_df), 1, 1))
  
write_csv(crime_df, 'data/processed_crime_data.csv')
# 
# 
# df_pfa_plot <- df_pfa %>%
#'   # mutate(reasonForSearch = case_when(
#'   #   reasonForSearch=='Terrorism Act 2000 s.43a'~'Terrorism',
#'   #   reasonForSearch=='Prevent acts of terrorism'~'Terrorism',
#'   #   reasonForSearch=='Stolen Property'~'Burglary, Robbery and Theft',
#'   #   reasonForSearch=='Going Equipped'~'Burglary, Robbery and Theft',
#'   #   reasonForSearch=='Other'~'Other discrete reasons (such as fireworks)',
#'   #   reasonForSearch=='Offensive Weapons'~'Offensive Weapons and Firearms',
#'   #   reasonForSearch=='Firearms'~'Offensive Weapons and Firearms',
#'   #   reasonForSearch=='Anticipation of violence'~'Anticipation of Violence',
#'   #   T~reasonForSearch
#'   # )) %>%
  # mutate(reasonForSearch = case_when(
  #   reasonForSearch=='Burglary, Robbery and Theft'~ 'Burglary, Robbery and Theft',
  #   reasonForSearch %in% c('Offensive Weapons and Firearms','Anticipation of Violence')~ 'Violent Crime',
  #   reasonForSearch == 'Drugs'~ 'Drugs Offences',
  #   reasonForSearch == 'Criminal Damage'~ 'Criminal Damage',
  #   T~'Drop'
  # )) %>%
  # filter(reasonForSearch!='Drop') %>%
  # select(year, financialYear, pfaName, numberOfSearches, totalSusceptibleCrime) %>%
  # group_by(year) %>%
  # mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
  # ungroup() %>%
  # distinct(pfaName, year, .keep_all=T) %>%
  # group_by(year) %>%
  # mutate(totalSusceptibleCrime = sum(totalSusceptibleCrime, na.rm=T)) %>%
  # ungroup() %>%
  # distinct(financialYear, .keep_all=T) %>%
  # mutate(across(c(numberOfSearches, totalSusceptibleCrime),~ (.x/60000000)*1000)) %>%
  # # mutate(numberOfSearches_l1 = lag(numberOfSearches, n=1)) %>%
  # # mutate(totalSusceptibleCrime_l1 = lag(totalSusceptibleCrime, n=1))%>%
  # mutate(numberOfSearches_t1 = numberOfSearches[1]) %>%
  # mutate(totalSusceptibleCrime_t1 = totalSusceptibleCrime[1]) %>%
  # # mutate(numberOfSearches_pc = (numberOfSearches-numberOfSearches_l1)/numberOfSearches_l1) %>%
  # # mutate(totalSusceptibleCrime_pc = (totalSusceptibleCrime-totalSusceptibleCrime_l1)/totalSusceptibleCrime_l1)
  # mutate(numberOfSearches_pc = (numberOfSearches-numberOfSearches_t1)/numberOfSearches_t1) %>%
  # mutate(totalSusceptibleCrime_pc = (totalSusceptibleCrime-totalSusceptibleCrime_t1)/totalSusceptibleCrime_t1)

#%>%
#'   # group_by(year) %>%
#'   # mutate(across(c(numberOfSearches, `totalSusceptibleCrime`),~ sum(.x, na.rm=T))) %>%
#'   # distinct(year, .keep_all=T) %>%
#'   # select(financialYear, numberOfSearches, totalSusceptibleCrime)
#' 
#' #1131369
#' 
#' 
# highchart() %>%
#   hc_xAxis(categories=df_pfa_plot$financialYear) %>%
#   hc_add_series(name='searches', df_pfa_plot$numberOfSearches_pc) %>%
#   hc_add_series(name='crime', df_pfa_plot$totalSusceptibleCrime_pc) %>%
#   hc_yAxis(title=list(text="% change from 2011/12"))
#' 
#' 
#' # % of searches versus % of crime
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' t <- left_join(df_pfa, crime_df, by=c('pfaName', 'financialYear')) 
#' t2 <- t %>% 
#'   mutate(reasonForSearch = case_when(
#'     reasonForSearch=='Burglary, Robbery and Theft'~ 'Burglary, Robbery and Theft',
#'     reasonForSearch %in% c('Offensive Weapons and Firearms','Anticipation of Violence')~ 'Violent Crime',
#'     reasonForSearch == 'Drugs'~ 'Drugs Offences',
#'     reasonForSearch == 'Criminal Damage'~ 'Criminal Damage',
#'     T~'Drop'
#'   )) %>%
#'   filter(reasonForSearch=='Violent Crime') %>%
#'   select(year, financialYear, pfaName, numberOfSearches, `Violent Crime`) %>%
#'   group_by(year) %>%
#'   mutate(numberOfSearches = sum(numberOfSearches, na.rm=T)) %>%
#'   ungroup() %>%
#'   distinct(pfaName, year, .keep_all=T) %>%
#'   group_by(year) %>%
#'   mutate(`Violent Crime` = sum(`Violent Crime`, na.rm=T)) %>%
#'   ungroup() %>%
#'   distinct(financialYear, .keep_all=T) %>%
#'   # mutate(numberOfSearches_l1 = lag(numberOfSearches, n=1)) %>%
#'   # mutate(totalSusceptibleCrime_l1 = lag(totalSusceptibleCrime, n=1))%>%
#'   mutate(numberOfSearches_t1 = numberOfSearches[1]) %>%
#'   mutate(violentCrime_t1 = `Violent Crime`[1]) %>%
#'   # mutate(numberOfSearches_pc = (numberOfSearches-numberOfSearches_l1)/numberOfSearches_l1) %>%
#'   # mutate(totalSusceptibleCrime_pc = (totalSusceptibleCrime-totalSusceptibleCrime_l1)/totalSusceptibleCrime_l1)
#'   mutate(numberOfSearches_pc = (numberOfSearches-numberOfSearches_t1)/numberOfSearches_t1) %>%
#'   mutate(violentCrime_pc = (`Violent Crime`-violentCrime_t1)/violentCrime_t1)
#' 
#' 
#' 
#' 
#' 
#' 
#' highchart() %>%
#'   hc_xAxis(categories=t2$financialYear) %>%
#'   hc_add_series(name='searches', t2$numberOfSearches_pc) %>%
#'   hc_add_series(name='violent crime', t2$violentCrime_pc)
#' 
#' 
#' 
#' 
#' # year-on-year percentage change
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' headline <- t2 %>%
#'   group_by(year) %>%
#'   mutate(searches = sum(numberOfSearches)) %>%
#'   ungroup() %>%
#'   distinct(year, .keep_all=T)
#' 
#' 
#'   filter(reasonForSearch) 
#' 
#' 
#' # merge by pfaName, financialYear
#' 
#' # filter to suspceitble searches only
#' 
#' # then sum searches by reasonForSearch,,pfa, year
#' 
#' # then plot, removing pfa grouping level
#' 
#' # add all into Shiny
#' 
#' # and get rough of it working with widgets and stuff
#' 
#' # send a render on view object as conditional panel condition
#' 
#' # shade area user selected
#' 
#' 
#' # Begin putting plots together for ethnic disaprities
#' 
#' # Rewrite population code in R
#' 
#' # Create draft plots
#' 
#' # Refine and put all tgoether tomorrow
#' 
#' 
#' 
#' 
#' unique(crime_df$pfaName) %in% unique(df_pfa$pfaName)
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' # merge into df_pfa 
#' 
#' 
#' 
#' 
#' # lapply that reads each year from each crime data file, filters to selected offences,
#' # and counts number of offences by pfa, and creates new grouping in line with above. Also make sure pfanames
#' # match 
#' 
#' t <- read_ods("data/crime data/prc-pfa-mar2013-mar2024-tabs.ods")
#' 
#' #' 
#' #' # 2013-05 onwards contains 'possession of weapons'
#' #' 
#' #' 
#' #' ###https://www.gov.uk/government/statistics/police-recorded-crime-open-data-tables
#' #' 
#' #' 
#' #' #===============================================================================
#' #' #             Download and unzip Police API Crime data
#' #' #===============================================================================
#' #' 
#' #' 
#' #' # December 2010 - December 2013 
#' #' #------------------------------------------------------------------------------
#' #' 
#' #' temp <- tempfile()
#' #' download.file("https://data.police.uk/data/archive/2013-12.zip",temp)
#' #' unzip(temp)
#' #' unlink(temp)
#' #' 
#' #' # Jan 2014-Present 
#' #' #-------------------------------------------------------------------------------
#' #' #'[Note.] The following procedure downloads and unzips >100 1GB+ files. This
#' #' #'is a computationally intensive operation and so multi-core threading is used to
#' #' #'(drastically) reduce run time. Change the number of cores specified in [Global Options] `CORES` in line 
#' #' #' with the specification/capability of your local machine. Using `7` cores on an 
#' #' #' Intel i7 results in a run-time of approximately 6 hours.
#' #' 
#' #' start <- proc.time
#' #' clusters <- parallel::makeCluster(CORES)
#' #' parallel::parSapply(
#' #'   clusters, DATES, function(d) {
#' #'     temp <- tempfile()
#' #'     print(paste0('Downloading and unzipping crime data for ',d))
#' #'     download.file(paste0("https://data.police.uk/data/archive/",d,".zip"),temp)
#' #'     unzip(temp)
#' #'     unlink(temp)
#' #'   }
#' #' )
#' #' stopCluster(clusters)
#' #' end <- proc.time()
#' #' print(paste0('Run time = ',end - start)) 
#' #' 
#' #' 
#' #' #===============================================================================
#' #' #              Load, clean and append Police API crime data
#' #' #===============================================================================
#' #' 
#' #' 
#' #' # Load and summarise monthly PFA crime datasets using multi-core threading
#' #' #-------------------------------------------------------------------------
#' #' #'[Note.] See the above explanation above (line 50) on choosing the appropriate number
#' #' #' of cores for multi-threading. This procedure is far less computationally intensive
#' #' #' than the previous, and so less cores can be used without a substantive hit to run-time.
#' #' 
#' #' # Get file paths (excluding British Transport Police)
#' #' file_paths <- list.files(getwd(), pattern = "*street.csv", recursive = T)
#' #' file_paths <- file_paths[!grepl('btp', file_paths)] 
#' #' # Initialise parallel (loading required libraries and local environment)
#' #' clusters <- parallel::makeCluster(CORES)
#' #' parallel::clusterEvalQ(clusters, {library(tidyverse); library(janitor); library(stringr)})
#' #' clusterExport(clusters, varlist=c("SUSCEPTIBLE_CRIMES"), envir=environment())
#' #' # Loop through files
#' #' crime_df__list <- parallel::parLapply(
#' #'   clusters, file_paths, function(file) {
#' #'     # Create year/month identifiers
#' #'     year <- substr(file, start=1, stop=4)
#' #'     month <- substr(file, start=6, stop=7)
#' #'     # Load data
#' #'     df <- read_csv(paste0(getwd(),'/',file)) %>% janitor::clean_names()
#' #'     # If crime_type is variable name in df, do the following:
#' #'     # 1. Get count of susceptible crimes
#' #'     # 2. Return report on data availability
#' #'     if ('crime_type' %in% names(df)) {
#' #'       df <- df %>%
#' #'         mutate(crime_type = stringr::str_to_title(crime_type)) %>%
#' #'         filter(crime_type %in% SUSCEPTIBLE_CRIMES) %>%
#' #'         filter(reported_by == falls_within) %>%
#' #'         count(crime_type, falls_within) %>%
#' #'         mutate(year = year, month=month) %>%
#' #'         rename(pfaName=falls_within, crime=crime_type, numberOfCrimes=n) %>%
#' #'         select(year, month, pfaName, crime, numberOfCrimes)
#' #'       df_report <- data.frame(
#' #'         name=file,
#' #'         missing_flag=0
#' #'       )
#' #'       return(list(df, df_report))
#' #'     }
#' #'     # # If crime_type is NOT variable name in df, do the following:
#' #'     # 1.  Return report on data availability
#' #'     else {
#' #'       df_report <- data.frame(
#' #'         name=file,
#' #'         missing_flag=1
#' #'       )
#' #'       return(list(NULL, df_report))
#' #'     }
#' #'   }
#' #' )
#' #' stopCluster(clusters)
#' #' # Check if any crime data is missing
#' #' report_df <- bind_rows(lapply(crime_df__list, function(list) list[2]))
#' #' print(paste0('Crime data is missing in ',sum(report_df$missing_flag==1), ' dataframes'))
#' #' 
#' #' 
#' #' # Append nested dataframes and reformat by financial year
#' #' #------------------------------------------------------------------------------
#' #' 
#' #' # Append
#' #' crime_df <- bind_rows(lapply(crime_df__list, function(list) list[1]))
#' #' # 
#' #' # starting from 
#' #' crime_df <- crime_df %>%
#' #'   mutate(across(c(year, month),~ as.numeric(as.character(.x)))) %>%
#' #'   mutate(year_alt = case_when(month>3~year+1, T~year-1)) %>%
#' #'   group_by(year, month) %>%
#' #'   mutate(financialYear = paste0(min(c(year, year_alt)),'/',max(c(year,year_alt)))) %>%
#' #'   ungroup() %>%
#' #'   relocate(financialYear, .before=pfaName) %>%
#' #'   select(-c(year, year_alt, month)) %>%
#' #'   mutate(susceptibleCrime = case_when(
#' #'     crime%in%SUSCEPTIBLE_CRIME_GROUPS$Drugs~ 'Drugs',
#' #'     crime%in%SUSCEPTIBLE_CRIME_GROUPS$`Violent Crime`~ 'Violent Crime',
#' #'     crime%in%SUSCEPTIBLE_CRIME_GROUPS$`Burglary, Robbery and Theft`~ 'Burglary, Robbery and Theft',
#' #'     crime%in%SUSCEPTIBLE_CRIME_GROUPS$`Criminal Damage`~ 'Criminal Damage'
#' #'   )) %>%
#' #'   group_by(financialYear, pfaName, susceptibleCrime) %>%
#' #'   summarise(numberOfCrimes = sum(numberOfCrimes)) %>%
#' #'   ungroup()
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' #crime_df <- bind_rows(crime_df__list)
#' #' 
#' #' 
#' #' 
#' #' folder_paths <- list.dirs(getwd(),recursive = F)  
#' #' parallelise <- parallel::makeCluster(CORES)
#' #' 
#' #' crime_df__list <- parallel::parLapply(
#' #'   parallelise, folder_paths, function(folder) {
#' #'     pfa_files <- list.files(folder, pattern = "*street.csv", recursive = F)  
#' #'     pfa_files <- pfa_files[!grepl('btp', pfa_files)]
#' #'     files__list <- lapply(
#' #'       pfa_files, function(file) {
#' #'         year_month <- substr(file, start=1, stop=7)
#' #'         year <- substr(year_month, start=1, stop=4)
#' #'         month <- substr(year_month, start=6, stop=7)
#' #'         tempdf <- read_csv(paste0(folder,'/',file)) %>% janitor::clean_names()
#' #'         if ('crime_type' %in% names(tempdf)) {
#' #'           tempdf <- tempdf %>%
#' #'             mutate(crime_type = stringr::str_to_title(crime_type)) %>%
#' #'             filter(crime_type %in% SUSCEPTIBLE_CRIMES ) %>%
#' #'             filter(reported_by == falls_within) %>%
#' #'             count(crime_type, falls_within) %>%
#' #'             mutate(year = year, month=month) %>%
#' #'             rename(pfaName=falls_within, crime=crime_type) %>%
#' #'             select(year, month, pfaName, crime, n)
#' #'           return(tempdf)
#' #'         }
#' #'         else {
#' #'           return(NULL)
#' #'         }
#' #'       }
#' #'     )
#' #'     df_month <- bind_rows(files__list)
#' #'     return(df_month)
#' #'   }
#' #' )
#' #' stopCluster(parallel)
#' #' crime_df <- bind_rows(crime_df__list)
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #'     
#' #'     for(f in files__list) {
#' #'       print(length(names(files__list[[f]])))
#' #'     }
#' #'     
#' #'     bind_rows
#' #'     
#' #'   
#' #'     
#' #'     }
#' #' )
#' #' 
#' #' 
#' #' 
#' #' files <- list.files(path = "./data/", pattern = "*.csv")
#' #' 
#' #' 
#' #' folder <- folders[1]
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' # t <- parallel::parSapply(cores,
#' #' #                     DATES, function(d) {
#' #' #                       temp <- tempfile()
#' #' #                       print(paste0('Downloading and unzipping crime data for ',d))
#' #' #                       
#' #' #                     }
#' #' # )
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' for (y in YEARS) {
#' #'   for(m in MONTHS) {
#' #'     temp <- tempfile()
#' #'     #if (url.exists(paste0("https://data.police.uk/data/archive/",y,"-",m,".zip"))==T) {
#' #'       print(paste0('Downloading and unzipping crime data for ',y,'-',m))
#' #'       download.file(paste0("https://data.police.uk/data/archive/",y,"-",m,".zip"),temp)
#' #'       unzip(temp)
#' #'       unlink(temp)
#' #'    # }
#' #'     #else {
#' #'     #  print(paste0('Crime data for ',y,'-',m,' has not been released'))
#' #'    # }
#' #' 
#' #'   }
#' #' }
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' t <- unlist(lapply(
#' #'   YEARS, function(y) {
#' #'     unlist(lapply(
#' #'       MONTHS, function(m) {
#' #'         print(paste0('Downloading and unzipping crime data for ',y,'-',m))
#' #'       }
#' #'     ))
#' #'   }
#' #' ))
#' #' 
#' #' # see if can accelerate procedure by removing file that won't be used, and then unzipping!
#' #' 
#' #' 
#' #' 
#' #' for (y in YEARS) {
#' #'   for(m in MONTHS) {
#' #'     print(paste0(y,"-",m))
#' #'   }
#' #' }
#' #' 
#' #' 
#' #' 
#' #' 
#' #' 
#' #' # https://data.police.uk/data/archive/2014-01.zip
#' #' # https://data.police.uk/data/archive/2024-04.zip
#' #' 
#' #' 
#' #' #https://search.r-project.org/CRAN/refmans/RCurl/html/url.exists.html check if exists
#' #' 
#' #' 
#' #' 
#' #' unlink(temp)
#' #' 
#' #' 
#' #' unzip("D:/blah/text.zip")
