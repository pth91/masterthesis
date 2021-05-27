source("../../helper/package_check.R")
source("../../helper/data.R")
source('./estimators.R')
source('./parametric_gpd.R')
load_idl_complete()

#-------------------------------------------------------------------------------

countries <- unique(idl_complete$DCOUNTRY)

# has to be done to delete the `.` death country
countries <- countries[nchar(countries) > 1]

countries <- countries[
  countries != "ITA" & countries != "LBN" & countries != "FIN" & countries != "SWE"
]

#-------------------------------------------------------------------------------

data_germany <- idl_complete[idl_complete$DCOUNTRY == "DEU", ]
data_france <- idl_complete[idl_complete$DCOUNTRY == "FRA", ]

data_germany$DDATE <- substr(data_germany$DDATE, 7, 10)
data_france$DDATE <- substr(data_france$DDATE, 7, 10)

death_years_germany <- sort(unique(data_germany$DDATE))
death_years_france <- sort(unique(data_france$DDATE))

unique_observations_germany <- {
  data_germany %>%
    group_by(DDATE, SEX) %>%
    summarize("observations" = n()) %>%
    arrange(desc(observations))
}
saveRDS(
  unique_observations_germany, 
  file = "../../../data/RData/unique_observations_germany.RData"
)


 unique_observations_france <- {
  data_france %>%
    group_by(DDATE, SEX) %>%
    summarize("observations" = n()) %>%
    arrange(desc(observations))
 }
saveRDS(
  unique_observations_france,
  file ="../../../data/RData/unique_observations_france.RData"
)

estimators_germany <- data.frame(
  COUNTRY = character(),
  DDATE = character(),
  N_OBSERVATIONS = integer(),
  SEX = character(),
  THRESHOLD = double(),
  OLDEST = double(),
  mle_scale = double(),
  mle_shape = double(),
  est_endpoint_d = double(),
  est_endpoint_Y = double(),
  eeh19_hazard_1 = double(),
  eeh19_hazard_2 = double(),
  eeh19_hazard_3 = double(),
  eeh19_hazard_4 = double(),
  eeh19_hazard_5 = double(),
  mean_res_life_d = double(),
  mean_res_life_Y = double()
)

for (death_year in death_years_germany) {
  women <- data_germany[data_germany$SEX == "F", ]
  men <- data_germany[data_germany$SEX == "M", ]
  women_agedays <- women[women$DDATE == death_year, ]
  women_agedays <- as.numeric(women_agedays$AGEDAYS)
  men_agedays <- men[men$DDATE == death_year, ]
  men_agedays <- as.numeric(men_agedays$AGEDAYS)

  if (length(women_agedays) > 1) {
    observations <- length(women_agedays)
    threshold <- min(women_agedays)
    oldest <- max(women_agedays)
    estimators_women <- mle_estimator(women_agedays)
    if (estimators_women != 0 && estimators_women[2] < 0) {
      mle_scale <- as.numeric(estimators_women[1])
      mle_shape <- as.numeric(estimators_women[2])
      est_endpoint_d <- endpoint_estimator(threshold, mle_scale, mle_shape)
      est_endpoint_Y <- est_endpoint_d / 365
      t <- c(est_endpoint_d - 365, est_endpoint_d - 273, est_endpoint_d - 181, est_endpoint_d - 90, est_endpoint_d - 1)
      direct_hazard <- einmahl_direct_hazard(mle_shape, est_endpoint_d, t)
      mean_residual_life_d <- mean_res_life(mle_shape, mle_scale, threshold, threshold)
      mean_residual_life_Y <- mean_residual_life_d / 365
      estimators_germany[nrow(estimators_germany) + 1, ] <- c("GER", death_year, observations, "F", threshold, oldest, mle_scale, mle_shape, est_endpoint_d, est_endpoint_Y, direct_hazard[1], direct_hazard[2], direct_hazard[3], direct_hazard[4], direct_hazard[5], mean_residual_life_d, mean_residual_life_Y)
    }
    else {
      estimators_germany[nrow(estimators_germany) + 1, ] <- c("GER", death_year, observations, "F", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    }
  }
  else {
    estimators_germany[nrow(estimators_germany) + 1, ] <- c("GER", death_year, observations, "F", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  }

  if (length(men_agedays) > 1) {
    observations <- length(men_agedays)
    threshold <- min(men_agedays)
    oldest <- max(men_agedays)
    estimators_men <- mle_estimator(men_agedays)
    if (estimators_men != 0 && estimators_men[2] < 0) {
      mle_scale <- estimators_men[1]
      mle_shape <- estimators_men[2]
      est_endpoint_d <- endpoint_estimator(threshold, mle_scale, mle_shape)
      est_endpoint_Y <- est_endpoint_d / 365
      t <- c(est_endpoint_d - 365, est_endpoint_d - 273, est_endpoint_d - 181, est_endpoint_d - 90, est_endpoint_d - 1)
      direct_hazard <- einmahl_direct_hazard(mle_shape, est_endpoint_d, t)
      mean_residual_life_d <- mean_res_life(mle_shape, mle_scale, threshold, threshold)
      mean_residual_life_Y <- mean_residual_life_d / 365
      estimators_germany[nrow(estimators_germany) + 1, ] <- c("GER", death_year, observations, "M", threshold, oldest, mle_scale, mle_shape, est_endpoint_d, est_endpoint_Y, direct_hazard[1], direct_hazard[2], direct_hazard[3], direct_hazard[4], direct_hazard[5], mean_residual_life_d, mean_residual_life_Y)
    }
    else {
      estimators_germany[nrow(estimators_germany) + 1, ] <- c("GER", death_year, observations, "M", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    }
  }
  else {
    estimators_germany[nrow(estimators_germany) + 1, ] <- c("GER", death_year, observations, "M", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  }
}
saveRDS(
  estimators_germany,
  file = "../../../data/RData/estimators_germany.RData"
)

estimators_france <- data.frame(
  COUNTRY = character(),
  DDATE = character(),
  N_OBSERVATIONS = integer(),
  SEX = character(),
  THRESHOLD = double(),
  OLDEST = double(),
  mle_scale = double(),
  mle_shape = double(),
  est_endpoint_d = double(),
  est_endpoint_Y = double(),
  eeh19_hazard_1 = double(),
  eeh19_hazard_2 = double(),
  eeh19_hazard_3 = double(),
  eeh19_hazard_4 = double(),
  eeh19_hazard_5 = double(),
  mean_res_life_d = double(),
  mean_res_life_Y = double()
)

for (death_year in death_years_france) {
  women <- data_france[data_france$SEX == "F", ]
  men <- data_france[data_france$SEX == "M", ]
  women_agedays <- women[women$DDATE == death_year, ]
  women_agedays <- as.numeric(women_agedays$AGEDAYS)
  men_agedays <- men[men$DDATE == death_year, ]
  men_agedays <- as.numeric(men_agedays$AGEDAYS)

  if (length(women_agedays) > 1) {
    observations <- length(women_agedays)
    threshold <- min(women_agedays)
    oldest <- max(women_agedays)
    estimators_women <- mle_estimator(women_agedays)
    if (estimators_women != 0 && estimators_women[2] < 0) {
      mle_scale <- as.numeric(estimators_women[1])
      mle_shape <- as.numeric(estimators_women[2])
      est_endpoint_d <- endpoint_estimator(threshold, mle_scale, mle_shape)
      est_endpoint_Y <- est_endpoint_d / 365
      t <- c(est_endpoint_d - 365, est_endpoint_d - 273, est_endpoint_d - 181, est_endpoint_d - 90, est_endpoint_d - 1)
      direct_hazard <- einmahl_direct_hazard(mle_shape, est_endpoint_d, t)
      mean_residual_life_d <- mean_res_life(mle_shape, mle_scale, threshold, threshold)
      mean_residual_life_Y <- mean_residual_life_d / 365
      estimators_france[nrow(estimators_france) + 1, ] <- c("FRA", death_year, observations, "F", threshold, oldest, mle_scale, mle_shape, est_endpoint_d, est_endpoint_Y, direct_hazard[1], direct_hazard[2], direct_hazard[3], direct_hazard[4], direct_hazard[5], mean_residual_life_d, mean_residual_life_Y)
    }
    else {
      estimators_france[nrow(estimators_france) + 1, ] <- c("FRA", death_year, observations, "F", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    }
  }
  else {
    estimators_france[nrow(estimators_france) + 1, ] <- c("FRA", death_year, observations, "F", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  }

  if (length(men_agedays) > 1) {
    observations <- length(men_agedays)
    threshold <- min(men_agedays)
    oldest <- max(men_agedays)
    estimators_men <- mle_estimator(men_agedays)
    if (estimators_men != 0 && estimators_men[2] < 0) {
      mle_scale <- estimators_men[1]
      mle_shape <- estimators_men[2]
      est_endpoint_d <- endpoint_estimator(threshold, mle_scale, mle_shape)
      est_endpoint_Y <- est_endpoint_d / 365
      t <- c(est_endpoint_d - 365, est_endpoint_d - 273, est_endpoint_d - 181, est_endpoint_d - 90, est_endpoint_d - 1)
      direct_hazard <- einmahl_direct_hazard(mle_shape, est_endpoint_d, t)
      mean_residual_life_d <- mean_res_life(mle_shape, mle_scale, threshold, threshold)
      mean_residual_life_Y <- mean_residual_life_d / 365
      estimators_france[nrow(estimators_france) + 1, ] <- c("FRA", death_year, observations, "M", threshold, oldest, mle_scale, mle_shape, est_endpoint_d, est_endpoint_Y, direct_hazard[1], direct_hazard[2], direct_hazard[3], direct_hazard[4], direct_hazard[5], mean_residual_life_d, mean_residual_life_Y)
    }
    else {
      estimators_france[nrow(estimators_france) + 1, ] <- c("FRA", death_year, observations, "M", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    }
  }
  else {
    estimators_france[nrow(estimators_france) + 1, ] <- c("FRA", death_year, observations, "M", threshold, oldest, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  }
}
saveRDS(estimators_france,file ="../../../data/RData/estimators_france.RData")
