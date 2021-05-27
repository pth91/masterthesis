source("../../helper/package_check.R")
source("../../helper/data.R")
library("writexl")
library("survival")
library('mice')
library('muhaz')
load_idl_complete()

#-------------------------------------------------------------------------------

countries <- unique(idl_complete$DCOUNTRY)

# has to be done to delete the `.` death country
countries <- countries[nchar(countries) > 1]

# TODO: auf jeden fall muss ich noch irgendwo schreiben, wieso ich gerade diese
# vier rausggenommmen habe...
# es gibt nur frauen und davon zu wenige beobachtungen
# TODO: EW ist das dcountry bei leuten die in UK geboren sind... wofür soll das
# stehen?
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

mle_estimator <- function(agedays) {
  tryCatch(
    {
      parameters <- fpot(agedays, threshold = min(agedays))
      return(parameters$param)
    },
    error = function(condition) {
      return(0)
    }
  )
}

endpoint_estimator <- function(threshold, mle_scale, mle_shape) {
  return(threshold - mle_scale / mle_shape)
}

einmahl_direct_hazard <- function(mle_shape, est_endpoint, x) {
  return(1 / (-mle_shape * (est_endpoint - x)))
}

gpd_hazard <- function(mle_shape, mle_scale, threshold, x) {
  return(1 / (mle_scale + mle_shape * (x - threshold)))
}

gpd_cum_hazard <- function(mle_shape, mle_scale, threshold, x) {
  # return((1/mle_shape) * (log(mle_scale + mle_shape * (x - threshold)) - log(mle_scale - mle_shape * threshold)))
  return((1 / mle_shape) * (log(mle_scale + mle_shape * (x - threshold)) - log(mle_scale)))
}

gpd_survival <- function(mle_shape, mle_scale, threshold, x) {
  return((1 + mle_shape * (x - threshold) / mle_scale)^(-1 / mle_shape))
}

mean_res_life <- function(mle_shape, mle_scale, threshold, x) {
  nominator <- -(mle_scale^(1 / mle_shape)) / (mle_shape - 1) * (mle_scale + mle_shape * (x - threshold))^(1 - 1 / mle_shape)
  denominator <- gpd_survival(mle_shape, mle_scale, threshold, x)
  return(nominator / denominator)
}


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
#ker <- function(x, obs, bw) {
#  return((1 / sqrt(2 * pi)) * exp(-1 / 2 * ((x - obs) / bw)^2))
#}
#khe <- function(bw, nobs, obs, x) {
#  ret <- 0
#  for (it in seq(1, nobs)) {
#    ret <- ret + 1 / (nobs - it + 1) * ker(x, obs[it], bw)
#  }
#  return(1 / bw * ret)
#}
#opt_bw <- function(bw, ui, obs, nobs) {
#  first <- 0
#  for (it in seq(0, length(ui) - 1)) {
#    first <- (u[it + 1] - u[it]) / 2 * (khe(bw, nobs, obs, u[it])^2 + khe(bw, nobs, obs, u[it + 1])^2)
#  }
#  second <- 0
#  for (iti in seq(0, length(obs))) {
#    for (itj in seq(0, length(obs))) {
#      if (iti != itj) {
#        second <- ker(obs[iti], obs[itj], bw) * (1 / (nobs - iti + 1)) * (1 / (nobs - itj + 1))
#      }
#    }
#  }
#  g <- first - 2 / bw * second
#  return(g)
#}
#
## fra <- data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"),]
## ad <- sort(fra$AGEDAYS)
## x <- khe(1, length(ad), ad, min(ad))
## u <- seq(min(ad), 45809, length.out = 100)
## f <- optim(1000, opt_bw, ui = u, obs = ad, nobs = length(ad))
## fra <- data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"),]
## ad <- sort(fra$AGEDAYS)
## x <- khe(1, length(ad), ad, min(ad))
## u <- seq(min(ad), 45809, length.out = 100)
## f <- optim(1000, opt_bw, ui = u, obs = ad, nobs = length(ad))
#fra <- data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"), ]
#ad <- sort(fra$AGEDAYS)
#u <- seq(min(ad), 45809, length.out = 100)
##f <- optim(1000, opt_bw, ui = u, obs = ad, nobs = length(ad))
#ger_women <- estimators_germany[which(estimators_germany$SEX == "F" & estimators_germany$DDATE == "2001"), ]
#ger_thresh_women <- as.numeric(ger_women$THRESHOLD)
#ger_oldest_women <- as.numeric(ger_women$OLDEST)
#ger_obs_women <- seq(ger_thresh_women, as.numeric(ger_women$est_endpoint_d) - 365)
## ger_obs_women <- seq(ger_thresh_women, ger_oldest_women + 365)
#ger_shape_women <- as.numeric(ger_women$mle_shape)
#ger_scale_women <- as.numeric(ger_women$mle_scale)
#ger_haz_women <- gpd_hazard(ger_shape_women, ger_scale_women, ger_thresh_women, ger_obs_women)
#ger_haz_einmahl_women <- einmahl_direct_hazard(ger_shape_women, as.numeric(ger_women$est_endpoint_d), ger_obs_women)
#ger_w <- data_germany[which(data_germany$DDATE == "2001" & data_germany$SEX == "F"), ]
#ad_ger_w <- sort(ger_w$AGEDAYS)
#u_ger_w <- seq(min(ad_ger_w), as.numeric(ger_women$est_endpoint_d), length.out = 100)
##f_ger_w <- optim(1000, opt_bw, ui = u_ger_w, obs = ad_ger_w, nobs = length(ad_ger_w))
##ger_khe_women <- khe(f_ger_w$par, length(ad_ger_w), ad_ger_w, ger_obs_women)
#ger_mu_women <- muhaz(ad_ger_w[1:(length(ad_ger_w) - 1)], bw.method = "g", min.time = ger_thresh_women, max.time = as.numeric(ger_women$est_endpoint_d) - 365)
#
#ger_men <- estimators_germany[which(estimators_germany$SEX == "M" & estimators_germany$DDATE == "2000"), ]
#ger_thresh_men <- as.numeric(ger_men$THRESHOLD)
#ger_oldest_men <- as.numeric(ger_men$OLDEST)
#ger_obs_men <- seq(ger_thresh_men, as.numeric(ger_men$est_endpoint_d) - 365)
## ger_obs_men <- seq(ger_thresh_men, ger_oldest_men + 365)
#ger_shape_men <- as.numeric(ger_men$mle_shape)
#ger_scale_men <- as.numeric(ger_men$mle_scale)
#ger_haz_men <- gpd_hazard(ger_shape_men, ger_scale_men, ger_thresh_men, ger_obs_men)
#ger_haz_einmahl_men <- einmahl_direct_hazard(ger_shape_men, as.numeric(ger_men$est_endpoint_d), ger_obs_men)
#ger_m <- data_germany[which(data_germany$DDATE == "2000" & data_germany$SEX == "M"), ]
#ad_ger_m <- sort(ger_m$AGEDAYS)
#u_ger_m <- seq(min(ad_ger_m), as.numeric(ger_men$est_endpoint_d), length.out = 100)
##f_ger_m <- optim(1000, opt_bw, ui = u_ger_m, obs = ad_ger_m, nobs = length(ad_ger_m))
##ger_khe_men <- khe(f_ger_m$par, length(ad_ger_m), ad_ger_m, ger_obs_men)
#ger_mu_men <- muhaz(ad_ger_m,  min.time = ger_thresh_men, max.time = as.numeric(ger_men$est_endpoint_d) - 365)
#
#pdf("./code/estimation/plots/hazard_rate_germany.pdf", width=14, height=8.6)
##png("./code/estimation/plots/hazard_rate_germany.png", width = 8, height = 4.3, units = "in", res=500)
##par(mfrow = c(1, 2))
#plot(ger_obs_women, ger_haz_women, main = "German Women, 2001", xlab = "Days Lived", ylab = "Estimated Hazrd Rate", "l")
#lines(ger_obs_women, ger_haz_einmahl_women, col = "red")
#lines(ger_mu_women$est.grid, ger_mu_women$haz.est, col = "blue")
#abline(v = ger_oldest_women, col = 'yellow')
#abline(v = ad_ger_w[length(ad_ger_w) - 1], col = 'green')
#abline(v = ad_ger_w[length(ad_ger_w) - 2], col = 'grey')
##axis(1, at = c(ger_thresh_women, ger_oldest_women))
#legend("topleft", c("Kernel Estimator", "Direct GPD"), fill = c("blue", "red"))
##plot(ger_obs_men, ger_haz_men, main = "German Men, 2000", xlab = "Days Lived", ylab = "", "l")
##lines(ger_obs_men, ger_haz_einmahl_men, col = "red")
##lines(ger_mu_men$est.grid, ger_mu_men$haz.est, col = "blue")
##abline(v = ger_oldest_men, col = 'yellow')
##abline(v = ad_ger_m[length(ad_ger_m) - 1], col = 'green')
##abline(v = ad_ger_m[length(ad_ger_m) - 2], col = 'grey')
##legend("bottomright", c("Kernel Estimator", "Direct GPD"), fill = c("blue", "red"))
##axis(1, at = c(ger_thresh_men, ger_oldest_men))
#dev.off()
#
#fra_women <- estimators_france[which(estimators_france$SEX == "F" & estimators_france$DDATE == "2017"), ]
#fra_thresh_women <- as.numeric(fra_women$THRESHOLD)
#fra_oldest_women <- as.numeric(fra_women$OLDEST)
#fra_obs_women <- seq(fra_thresh_women, as.numeric(fra_women$est_endpoint_d) - 365)
## fra_obs_women <- seq(fra_thresh_women, fra_oldest_women + 365)
#fra_shape_women <- as.numeric(fra_women$mle_shape)
#fra_scale_women <- as.numeric(fra_women$mle_scale)
#fra_haz_women <- gpd_hazard(fra_shape_women, fra_scale_women, fra_thresh_women, fra_obs_women)
#fra_mu_women <- muhaz(ad, bw.method = "g", min.time = fra_thresh_women, max.time = as.numeric(fra_women$est_endpoint_d) - 365)
#fra_haz_einmahl_women <- einmahl_direct_hazard(fra_shape_women, as.numeric(fra_women$est_endpoint_d), fra_obs_women)
##fra_women_khe <- khe(692, length(ad), ad, fra_obs_women)
#
#fra_men <- estimators_france[which(estimators_france$SEX == "M" & estimators_france$DDATE == "2015"), ]
#fra_thresh_men <- as.numeric(fra_men$THRESHOLD)
#fra_oldest_men <- as.numeric(fra_men$OLDEST)
#fra_obs_men <- seq(fra_thresh_men, as.numeric(fra_men$est_endpoint_d) - 365)
## fra_obs_men <- seq(fra_thresh_men, fra_oldest_men + 365)
#fra_shape_men <- as.numeric(fra_men$mle_shape)
#fra_scale_men <- as.numeric(fra_men$mle_scale)
#fra_haz_men <- gpd_hazard(fra_shape_men, fra_scale_men, fra_thresh_men, fra_obs_men)
#fra_haz_einmahl_men <- einmahl_direct_hazard(fra_shape_men, as.numeric(fra_men$est_endpoint_d), fra_obs_men)
#fra_m <- data_france[which(data_france$DDATE == "2015" & data_france$SEX == "M"), ]
#ad_fra_m <- sort(fra_m$AGEDAYS)
#u_fra_m <- seq(min(ad_fra_m), as.numeric(fra_men$est_endpoint_d), length.out = 100)
#fra_mu_men <- muhaz(ad_fra_m, bw.method = "g", min.time = fra_thresh_men, max.time = as.numeric(fra_men$est_endpoint_d) - 365)
##f_fra_m <- optim(1000, opt_bw, ui = u_fra_m, obs = ad_fra_m, nobs = length(ad_fra_m))
##fra_khe_men <- khe(f_fra_m$par, length(ad_fra_m), ad_fra_m, fra_obs_men)
#library("muhaz")
##png("./code/estimation/plots/hazard_rate_france.png", width = 1920, height =1080)
#pdf("./code/estimation/plots/hazard_rate_france.pdf", width=14, height=8.6)
#par(mfrow = c(1, 2))
#plot(fra_obs_women, fra_haz_women, main = "French Women, 2017", xlab = "Days Lived", ylab = "Estimated Hazard Rate", "l")
#lines(fra_obs_women, fra_haz_einmahl_women, col = "red")
##lines(fra_obs_women, fra_women_khe, col = "green")
#lines(fra_mu_women$est.grid, fra_mu_women$haz.est, col = "blue")
#abline(v = fra_oldest_women, col = 'yellow')
#abline(v = ad[length(ad) - 1], col = 'green')
#abline(v = ad[length(ad) - 2], col = 'grey')
#legend("topleft", c("Kernel Estimator", "Direct GPD"), fill = c("blue", "red"))
##axis(1, at = c(fra_thresh_women, fra_oldest_women))
#plot(fra_obs_men, fra_haz_men, main = "French Men, 2015", xlab = "Days Lived", ylab = "", "l")
#lines(fra_obs_men, fra_haz_einmahl_men, col = "red")
#lines(fra_mu_men$est.grid, fra_mu_men$haz.est, col = "blue")
##lines(fra_obs_men, fra_khe_men, col = "green")
#abline(v = fra_oldest_men, col = 'yellow')
#abline(v = ad_fra_m[length(ad_fra_m) - 1], col = 'green')
#abline(v = ad_fra_m[length(ad_fra_m) - 2], col = 'grey')
#legend("topleft", c("Kernel Estimator", "Direct GPD"), fill = c("blue", "red"))
##axis(1, at = c(fra_thresh_men, fra_oldest_men))
#dev.off()
#
##png("./code/estimation/plots/kaplan-meier-survival_germany.png", width = 8, height = 4.3, units = "in", res=500)
#pdf("./code/estimation/plots/kaplan-meier-survival_germany.pdf", width =14 , height = 8.6)
#par(mfrow = c(1, 2))
#ger_agedays_women <- as.numeric(sort(data_germany[which(data_germany$DDATE == "2001" & data_germany$SEX == "F"), ]$AGEDAYS))
#ger_surv_women <- gpd_survival(ger_shape_women, ger_scale_women, ger_thresh_women, ger_agedays_women)
#ger_km_women <- survfit(Surv(ger_agedays_women) ~ 1)
#plot(ger_km_women, xlim = c(min(ger_agedays_women), max(ger_agedays_women)), main = "German Women, 2001", xlab = "Days Lived", ylab = "Estimated Survival Function", col = "blue")
#lines(ger_agedays_women, ger_surv_women, col = "red")
##axis(1, at = c(min(ger_agedays_women), max(ger_agedays_women)))
#legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
#ger_agedays_men <- as.numeric(sort(data_germany[which(data_germany$DDATE == "2000" & data_germany$SEX == "M"), ]$AGEDAYS))
#ger_surv_men <- gpd_survival(ger_shape_men, ger_scale_men, ger_thresh_men, ger_agedays_men)
#ger_km_men <- survfit(Surv(ger_agedays_men) ~ 1)
#plot(ger_km_men, xlim = c(min(ger_agedays_men), max(ger_agedays_men)), main = "German Men, 2000", xlab = "Days Lived", ylab = "", col = "blue")
#lines(ger_agedays_men, ger_surv_men, col = "red")
##axis(1, at = c(min(ger_agedays_men), max(ger_agedays_men)))
#legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
#dev.off()
##plot_na_germany <- ggplot(
##  NULL,
##  aes(x, y),
##  colour = y
##) +
##geom_line(data = df_mle_women, aes(x, y, colour = "Women")) +
##geom_line(data = df_mle_men, aes(x, y, colour = "Men")) +
##geom_hline(yintercept = -0.5, col = "orange") +
##labs(
##  x = "Year of Death",
##  y = unname(TeX("$\\hat{\\gamma}"))
##) +
##scale_color_manual(name = c('Women', 'Men'), values=c(Women = "red", Men = "blue"))+
##theme(
##  legend.position = c(0.15, 0.15),
##  legend.key.size = unit(0.5, "cm"),
##  legend.key.height = unit(0.5, "cm"),
##  legend.key.width = unit(0.5, "cm"),
##  legend.title = element_blank(),
##  plot.title = element_text(hjust = 0.5),
##  plot.subtitle = element_text(hjust = 0.5),
##  # panel.border = element_blank(),
##  panel.grid.major = element_line(color = "grey"),
##  # panel.grid.minor = element_blank(),
##  panel.background = element_blank(),
##  axis.line = element_line(color = "black"),
##  legend.text.align = 0
##)
##
##ggsave(
##  "code/estimation/plots/na_germany.png",
##  plot = plot_na_germany,
##  width = 7,
##  height = 4.3
##)
##png("./code/estimation/plots/kaplan-meier-survival_france.png", width = 8, height = 4.3, res = 500, units="in")
#pdf("./code/estimation/plots/kaplan-meier-survival_france.pdf", width = 14, height = 8.6)
#par(mfrow = c(1, 2))
#fra_agedays_women <- as.numeric(sort(data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"), ]$AGEDAYS))
#fra_surv_women <- gpd_survival(fra_shape_women, fra_scale_women, fra_thresh_women, fra_agedays_women)
#fra_km_women <- survfit(Surv(fra_agedays_women) ~ 1)
#plot(fra_km_women, xlim = c(min(fra_agedays_women), max(fra_agedays_women)), main = "French Women, 2017", xlab = "Days Lived", ylab = "Estimated Survival Function", col = "blue")
#lines(fra_agedays_women, fra_surv_women, col = "red")
##axis(1, at = c(min(fra_agedays_women), max(fra_agedays_women)))
#legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
#fra_agedays_men <- as.numeric(sort(data_france[which(data_france$DDATE == "2015" & data_france$SEX == "M"), ]$AGEDAYS))
#fra_surv_men <- gpd_survival(fra_shape_men, fra_scale_men, fra_thresh_men, fra_agedays_men)
#fra_km_men <- survfit(Surv(fra_agedays_men) ~ 1)
#plot(fra_km_men, xlim = c(min(fra_agedays_men), max(fra_agedays_men)), main = "French Men, 2015", xlab = "Days Lived", ylab = "", col = "blue")
#lines(fra_agedays_men, fra_surv_men, col = "red")
##axis(1, at = c(min(fra_agedays_men), max(fra_agedays_men)))
#legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
#dev.off()
#
#library("mice")
#ger_women_data <- data.frame(data = ger_agedays_women, time = ger_agedays_women)
#ger_women_data$status <- 1
#ger_women_na <- nelsonaalen(data = ger_women_data, timevar = time, statusvar = status)
#ger_women_gh <- gpd_cum_hazard(ger_shape_women, ger_scale_women, ger_thresh_women, ger_agedays_women)
#ger_men_data <- data.frame(data = ger_agedays_men, time = ger_agedays_men)
#ger_men_data$status <- 1
#ger_men_na <- nelsonaalen(data = ger_men_data, timevar = time, statusvar = status)
#ger_men_gh <- gpd_cum_hazard(ger_shape_men, ger_scale_men, ger_thresh_men, ger_agedays_men)
##png("./code/estimation/plots/nelson-aalen_germany.png", width = 8, height = 4.3, units ="in", res=500)
#pdf("./code/estimation/plots/nelson-aalen_germany.pdf", width = 14, height = 8.6)
#par(mfrow = c(1, 2))
#plot(ger_agedays_women, ger_women_na, "l", ylim = range(ger_women_na, ger_women_gh), main = "German Women, 2001", xlab = "Days Lived", ylab = "Estimated Cumulated Hazard Rate", col = "blue")
#lines(x = ger_agedays_women, y = ger_women_gh, col = "red")
#lines(unique(ad_ger_w), -log(ger_km_women$surv), col = 'green')
#legend("topleft", c("Nelson-Aalen", "Indirect GPD", "Direct GPD"), fill = c("blue", "green", "red"))
#plot(ger_agedays_men, ger_men_na, "l", ylim = range(ger_men_na, ger_men_gh), main = "German Men, 2000", xlab = "Days Lived", ylab = "", col = "blue")
#lines(x = ger_agedays_men, y = ger_men_gh, col = "red")
#lines(unique(ad_ger_m), -log(ger_km_men$surv), col = 'green')
#legend("topleft", c("Nelson-Aalen", "Indirect GPD", "Direct GPD"), fill = c("blue", "green", "red"))
#dev.off()
#
#fra_women_data <- data.frame(data = fra_agedays_women, time = fra_agedays_women)
#fra_women_data$status <- 1
#fra_women_na <- nelsonaalen(data = fra_women_data, timevar = time, statusvar = status)
#fra_women_gh <- gpd_cum_hazard(fra_shape_women, fra_scale_women, fra_thresh_women, fra_agedays_women)
#fra_men_data <- data.frame(data = fra_agedays_men, time = fra_agedays_men)
#fra_men_data$status <- 1
#fra_men_na <- nelsonaalen(data = fra_men_data, timevar = time, statusvar = status)
#fra_men_gh <- gpd_cum_hazard(fra_shape_men, fra_scale_men, fra_thresh_men, fra_agedays_men)
##png("./code/estimation/plots/nelson-aalen_france.png", width = 8, height = 4.3, units="in", res=500)
#pdf("./code/estimation/plots/nelson-aalen_france.pdf", width = 14, height = 8.6)
#par(mfrow = c(1, 2))
#plot(fra_agedays_women, fra_women_na, "l", ylim = range(fra_women_na, fra_women_gh), main = "French Women, 2017", xlab = "Days Lived", ylab = "Estimated Cumulated Hazard Rate", col = "blue")
#lines(x = fra_agedays_women, y = fra_women_gh, col = "red")
#lines(unique(ad), -log(fra_km_women$surv), col = 'green')
#legend("topleft", c("Nelson-Aalen", "Indirect GPD", "Direct GPD"), fill = c("blue", "green", "red"))
#plot(fra_agedays_men, fra_men_na, "l", ylim = range(fra_men_na, fra_men_gh), main = "French Men, 2015", xlab = "Days Lived", ylab = "", col = "blue")
#lines(x = fra_agedays_men, y = fra_men_gh, col = "red")
#lines(unique(ad_fra_m), -log(fra_km_men$surv), col = 'green')
#legend("topleft", c("Nelson-Aalen", "Indirect GPD", "Direct GPD"), fill = c("blue", "green", "red"))
#dev.off()
#
#tests <- data.frame(
#  negative_index_france = double(),
#  negative_index_germany = double(),
#  finite_endpoint_france = double(),
#  finite_endpoint_germany = double()
#)
#
#negative_index <- function(v_mle_shape, v_nobs){
#  return(sqrt(min(v_nobs)) * max(v_mle_shape))
#}
#
#finite_endpoint <- function(v_mle_shape, v_mle_scale, v_est_endpoint, v_nobs){
#  n = length(v_nobs)
#  variance <- function(mle_shape){
#    return(2 + 2 * mle_shape^(-1) + 5 * mle_shape^(-2) + 4 * mle_shape^(-3) + mle_shape^(-4))
#  }
#  d <- 0
#  for(it in seq(1, n)){
#    k <- v_nobs[it]
#    d <- d + k/(v_mle_scale[it] * variance(v_mle_shape[it]))^2
#  }
#  omega_tilde <- 0
#  for(it in seq(1, n)){
#    k <- v_nobs[it]
#    omega_tilde <- omega_tilde + k/(v_mle_scale[it] * variance(v_mle_shape[it]))^2 * 1/d * v_est_endpoint[it]
#  }
#  T <- 0
#  for(it in seq(1, n)){
#    T <- k/(v_mle_scale[it] * variance(v_mle_shape[it]))^2 * (v_est_endpoint[it] - omega_tilde)^2
#  }
#  T <- d * T
#  return(T)
#}
#
#ger_est_women <- estimators_germany[which(estimators_germany$SEX == "F"),]
#ger_est_women <- ger_est_women %>% drop_na()
#ger_women_mle_shape <- as.numeric(ger_est_women$mle_shape)
#ger_women_mle_scale <- as.numeric(ger_est_women$mle_scale)
#ger_women_est_endpoint <- as.numeric(ger_est_women$est_endpoint_d)
#ger_women_nobs <- as.numeric(ger_est_women$N_OBSERVATIONS)
#ger_neg_test_women <- negative_index(ger_women_mle_shape, ger_women_nobs)
#ger_finite_endp_women <- finite_endpoint(ger_women_mle_shape, ger_women_mle_scale, ger_women_est_endpoint, ger_women_nobs)
#ger_est_men <- estimators_germany[which(estimators_germany$SEX == "M"),]
#ger_est_men <- ger_est_men %>% drop_na()
#ger_men_mle_shape <- as.numeric(ger_est_men$mle_shape)
#ger_men_mle_scale <- as.numeric(ger_est_men$mle_scale)
#ger_men_est_endpoint <- as.numeric(ger_est_men$est_endpoint_d)
#ger_men_nobs <- as.numeric(ger_est_men$N_OBSERVATIONS)
#ger_neg_test_men <- negative_index(ger_men_mle_shape, ger_men_nobs)
#
#fra_est_women <- estimators_france[which(estimators_france$SEX == "F"),]
#fra_est_women <- fra_est_women %>% drop_na()
#fra_women_mle_shape <- as.numeric(fra_est_women$mle_shape)
#fra_women_mle_scale <- as.numeric(fra_est_women$mle_scale)
#fra_women_est_endpoint <- as.numeric(fra_est_women$est_endpoint_d)
#fra_women_nobs <- as.numeric(fra_est_women$N_OBSERVATIONS)
#fra_neg_test_women <- negative_index(fra_women_mle_shape, fra_women_nobs)
#fra_finite_endp_women <- finite_endpoint(fra_women_mle_shape, fra_women_mle_scale, fra_women_est_endpoint, fra_women_nobs)
#fra_est_men <- estimators_france[which(estimators_france$SEX == "M"),]
#fra_est_men <- fra_est_men %>% drop_na()
#fra_men_mle_shape <- as.numeric(fra_est_men$mle_shape)
#fra_men_mle_scale <- as.numeric(fra_est_men$mle_scale)
#fra_men_est_endpoint <- as.numeric(fra_est_men$est_endpoint_d)
#fra_men_nobs <- as.numeric(fra_est_men$N_OBSERVATIONS)
#fra_neg_test_men <- negative_index(fra_men_mle_shape, fra_men_nobs)
#
#png('./code/estimation/plots/neg_index_france.png', width=1920, height=1080)
#estimators_france_women <- estimators_france[estimators_france$SEX == "F",]
#estimators_france_women <- estimators_france_women %>% drop_na()
#estimators_france_men <- estimators_france[estimators_france$SEX == "M",]
#estimators_france_men <- estimators_france_men %>% drop_na()
#plot(as.numeric(estimators_france_women$DDATE),
#      as.numeric(estimators_france_women$mle_shape),
#      main="ML Estimators for French Women",
#      xlab = "Year of Death",
#      ylab = unname(TeX("$\\hat{\\gamma}")),
#      col = 'red',
#      'l'
#    )
#lines(as.numeric(estimators_france_men$DDATE),
#      as.numeric(estimators_france_men$mle_shape),
#      col = 'blue'
#      )
#abline(h=0, col='yellow')
#abline(h=-0.5, col='orange')
#legend("topleft", c("Women", "Men"), fill = c("red", "blue"))
#dev.off()
#
#png('./code/estimation/plots/neg_index_germany.png', width=1920, height=1080)
#estimators_german_women <- estimators_germany[estimators_germany$SEX == "F",]
#estimators_german_women <- estimators_german_women %>% drop_na()
#estimators_german_men <- estimators_germany[estimators_germany$SEX == "M",]
#estimators_german_men <- estimators_german_men %>% drop_na()
#plot(as.numeric(estimators_german_women$DDATE),
#      as.numeric(estimators_german_women$mle_shape),
#      main="ML Estimators for German Women",
#      xlab = "Year of Death",
#      ylab = unname(TeX("$\\hat{\\gamma}")),
#      col = 'red',
#      'l'
#    )
#lines(as.numeric(estimators_german_men$DDATE),
#      as.numeric(estimators_german_men$mle_shape),
#      col = 'blue'
#      )
#abline(h=0, col='yellow')
#abline(h=-0.5, col='orange')
#legend("topleft", c("Women", "Men"), fill = c("red", "blue"))
#dev.off()
#
#df_mle_women <- data.frame(
#  x = as.numeric(estimators_german_women$DDATE),
#  y = as.numeric(estimators_german_women$mle_shape)
#)
#df_mle_men <- data.frame(
#  x = as.numeric(estimators_german_men$DDATE),
#  y = as.numeric(estimators_german_men$mle_shape)
#)
#
#plot_mle_ger <- ggplot(
#  NULL,
#  aes(x, y),
#  colour = y
#) +
#geom_line(data = df_mle_women, aes(x, y, colour = "Women")) +
#geom_line(data = df_mle_men, aes(x, y, colour = "Men")) +
#geom_hline(yintercept = -0.5, col = "orange") +
#labs(
#  x = "Year of Death",
#  y = unname(TeX("$\\hat{\\gamma}"))
#) +
#scale_color_manual(name = c('Women', 'Men'), values=c(Women = "red", Men = "blue"))+
#theme(
#  legend.position = c(0.15, 0.15),
#  legend.key.size = unit(0.5, "cm"),
#  legend.key.height = unit(0.5, "cm"),
#  legend.key.width = unit(0.5, "cm"),
#  legend.title = element_blank(),
#  plot.title = element_text(hjust = 0.5),
#  plot.subtitle = element_text(hjust = 0.5),
#  # panel.border = element_blank(),
#  panel.grid.major = element_line(color = "grey"),
#  # panel.grid.minor = element_blank(),
#  panel.background = element_blank(),
#  axis.line = element_line(color = "black"),
#  legend.text.align = 0
#)
#
#ggsave(
#  "code/estimation/plots/mle_shape_germany.png",
#  plot = plot_mle_ger,
#  width = 7,
#  height = 4.3
#)
#
#df_mle_women <- data.frame(
#  x = as.numeric(estimators_france_women$DDATE),
#  y = as.numeric(estimators_france_women$mle_shape)
#)
#df_mle_men <- data.frame(
#  x = as.numeric(estimators_france_men$DDATE),
#  y = as.numeric(estimators_france_men$mle_shape)
#)
#
#plot_mle_france <- ggplot(
#  NULL,
#  aes(x, y),
#  colour = y
#) +
#geom_line(data = df_mle_women, aes(x, y, colour = "Women")) +
#geom_line(data = df_mle_men, aes(x, y, colour = "Men")) +
#geom_hline(yintercept = -0.5, col = "orange") +
#labs(
#  x = "Year of Death",
#  y = unname(TeX("$\\hat{\\gamma}"))
#) +
#scale_color_manual(name = c('Women', 'Men'), values=c(Women = "red", Men = "blue"))+
#theme(
#  legend.position = c(0.15, 0.15),
#  legend.key.size = unit(0.5, "cm"),
#  legend.key.height = unit(0.5, "cm"),
#  legend.key.width = unit(0.5, "cm"),
#  legend.title = element_blank(),
#  plot.title = element_text(hjust = 0.5),
#  plot.subtitle = element_text(hjust = 0.5),
#  # panel.border = element_blank(),
#  panel.grid.major = element_line(color = "grey"),
#  # panel.grid.minor = element_blank(),
#  panel.background = element_blank(),
#  axis.line = element_line(color = "black"),
#  legend.text.align = 0
#)
#
#ggsave(
#  "code/estimation/plots/mle_shape_france.png",
#  plot = plot_mle_france,
#  width = 7,
#  height = 4.3
#)
#