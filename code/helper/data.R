library(dplyr)
load_idl_complete <- function() {
  assign(
    "idl_complete",
    readRDS("../../../data/RData/idl_complete.RData"),
    envir = globalenv()
  )
}
load_estimations <- function(){
  assign(
    "estimators_france",
    readRDS("../../../data/RData/estimators_france.RData"),
    envir = globalenv()
  )
  assign(
    "estimators_germany",
    readRDS("../../../data/RData/estimators_germany.RData"),
    envir = globalenv()
  )
}

get_age_count_by_country <- function(country = "all") {
  if (country == "all") {
    df <- idl_complete %>%
      group_by(AGEYEARS, DCOUNTRY) %>%
      tally()
    return(df)
  }
  else {
    df <- idl_complete[which(idl_complete$DCOUNTRY == country), ] %>%
      group_by(AGEDAYS, DCOUNTRY) %>%
      tally()
    return(df)
  }
}

get_blockmaxima_by_dcountry <- function() {
  idl_complete <- load_idl_complete()
  idl_complete$BDATE <- substring(idl_complete$BDATE, first = 7)
  # blockmaxima_tmp <- idl_complete %>% select(AGEDAYS, DCOUNTRY)
  # blockmaxima_tmp <- idl_complete %>% select(AGEDAYS, AGEYEARS)
  blockmaxima_tmp <- idl_complete %>% select(AGEDAYS, BDATE)

  blockmaxima <- blockmaxima_tmp %>%
    # group_by(DCOUNTRY) %>%
    # group_by(AGEYEARS) %>%
    group_by(BDATE) %>%
    summarise(
      # sollte man hier wikrlich die na wegmachen?
      number = n(),
      MaxAGEDAYSByDCOUNTRY = max(AGEDAYS, na.rm = T),
      MinAGEDATSByDCOUNTRY = min(AGEDAYS, na.rm = T)
    ) %>%
    # arrange(DCOUNTRY)
    # arrange(AGEYEARS)
    arrange(BDATE)
  return(blockmaxima)
}

get_agedays_by_country <- function(country = "DEU") {
  df_men <- idl_complete[which(idl_complete$DCOUNTRY == country & idl_complete$SEX == "M"), ]
  return(df_men)
}
