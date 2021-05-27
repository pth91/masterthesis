source("../../helper/package_check.R")
source("../../helper/data.R")
source('./estimators.R')
source('./parametric_gpd.R')

load_idl_complete()

data_germany <- idl_complete[idl_complete$DCOUNTRY == "DEU", ]
data_france <- idl_complete[idl_complete$DCOUNTRY == "FRA", ]

data_germany$DDATE <- substr(data_germany$DDATE, 7, 10)
data_france$DDATE <- substr(data_france$DDATE, 7, 10)

load_estimations()

ger_women <- estimators_germany[which(estimators_germany$SEX == "F" & estimators_germany$DDATE == "2001"), ]
ger_thresh_women <- as.numeric(ger_women$THRESHOLD)
ger_oldest_women <- as.numeric(ger_women$OLDEST)
ger_obs_women <- seq(ger_thresh_women, as.numeric(ger_women$est_endpoint_d) - 365)
ger_shape_women <- as.numeric(ger_women$mle_shape)
ger_scale_women <- as.numeric(ger_women$mle_scale)
ger_haz_women <- gpd_hazard(ger_shape_women, ger_scale_women, ger_thresh_women, ger_obs_women)
ger_haz_einmahl_women <- einmahl_direct_hazard(ger_shape_women, as.numeric(ger_women$est_endpoint_d), ger_obs_women)
ger_w <- data_germany[which(data_germany$DDATE == "2001" & data_germany$SEX == "F"), ]
ad_ger_w <- sort(ger_w$AGEDAYS)
u_ger_w <- seq(min(ad_ger_w), as.numeric(ger_women$est_endpoint_d), length.out = 100)
ger_mu_women <- muhaz(ad_ger_w[1:(length(ad_ger_w) - 1)], bw.method = "g", min.time = ger_thresh_women, max.time = as.numeric(ger_women$est_endpoint_d) - 365)
ger_agedays_women <- as.numeric(sort(data_germany[which(data_germany$DDATE == "2001" & data_germany$SEX == "F"), ]$AGEDAYS))
ger_surv_women <- gpd_survival(ger_shape_women, ger_scale_women, ger_thresh_women, ger_agedays_women)
ger_km_women <- survfit(Surv(ger_agedays_women) ~ 1)
ger_women_data <- data.frame(data = ger_agedays_women, time = ger_agedays_women)
ger_women_data$status <- 1
ger_women_na <- nelsonaalen(data = ger_women_data, timevar = time, statusvar = status)
ger_women_gh <- gpd_cum_hazard(ger_shape_women, ger_scale_women, ger_thresh_women, ger_agedays_women)
ger_women_est_endpoint <- as.numeric(ger_women$est_endpoint_d)

ger_men <- estimators_germany[which(estimators_germany$SEX == "M" & estimators_germany$DDATE == "2000"), ]
ger_thresh_men <- as.numeric(ger_men$THRESHOLD)
ger_oldest_men <- as.numeric(ger_men$OLDEST)
ger_obs_men <- seq(ger_thresh_men, as.numeric(ger_men$est_endpoint_d) - 365)
ger_shape_men <- as.numeric(ger_men$mle_shape)
ger_scale_men <- as.numeric(ger_men$mle_scale)
ger_haz_men <- gpd_hazard(ger_shape_men, ger_scale_men, ger_thresh_men, ger_obs_men)
ger_haz_einmahl_men <- einmahl_direct_hazard(ger_shape_men, as.numeric(ger_men$est_endpoint_d), ger_obs_men)
ger_m <- data_germany[which(data_germany$DDATE == "2000" & data_germany$SEX == "M"), ]
ad_ger_m <- sort(ger_m$AGEDAYS)
u_ger_m <- seq(min(ad_ger_m), as.numeric(ger_men$est_endpoint_d), length.out = 100)
ger_mu_men <- muhaz(ad_ger_m,  min.time = ger_thresh_men, max.time = as.numeric(ger_men$est_endpoint_d) - 365)
ger_agedays_men <- as.numeric(sort(data_germany[which(data_germany$DDATE == "2000" & data_germany$SEX == "M"), ]$AGEDAYS))
ger_surv_men <- gpd_survival(ger_shape_men, ger_scale_men, ger_thresh_men, ger_agedays_men)
ger_km_men <- survfit(Surv(ger_agedays_men) ~ 1)
ger_men_data <- data.frame(data = ger_agedays_men, time = ger_agedays_men)
ger_men_data$status <- 1
ger_men_na <- nelsonaalen(data = ger_men_data, timevar = time, statusvar = status)
ger_men_gh <- gpd_cum_hazard(ger_shape_men, ger_scale_men, ger_thresh_men, ger_agedays_men)
ger_men_est_endpoint <- as.numeric(ger_men$est_endpoint_d)

fra_women <- estimators_france[which(estimators_france$SEX == "F" & estimators_france$DDATE == "2017"), ]
fra_thresh_women <- as.numeric(fra_women$THRESHOLD)
fra_oldest_women <- as.numeric(fra_women$OLDEST)
fra_obs_women <- seq(fra_thresh_women, as.numeric(fra_women$est_endpoint_d) - 365)
fra_shape_women <- as.numeric(fra_women$mle_shape)
fra_scale_women <- as.numeric(fra_women$mle_scale)
fra_haz_women <- gpd_hazard(fra_shape_women, fra_scale_women, fra_thresh_women, fra_obs_women)
fra_haz_einmahl_women <- einmahl_direct_hazard(fra_shape_women, as.numeric(fra_women$est_endpoint_d), fra_obs_women)
fra_w <- data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"), ]
ad_fra_w <- sort(fra_w$AGEDAYS)
u_fra_w <- seq(min(ad_fra_w), as.numeric(fra_women$est_endpoint_d), length.out = 100)
fra_mu_women <- muhaz(ad_fra_w, bw.method = "g", min.time = fra_thresh_women, max.time = as.numeric(fra_women$est_endpoint_d) - 365)
fra_women <- estimators_france[which(estimators_france$SEX == "F" & estimators_france$DDATE == "2017"), ]
fra_thresh_women <- as.numeric(fra_women$THRESHOLD)
fra_oldest_women <- as.numeric(fra_women$OLDEST)
fra_obs_women <- seq(fra_thresh_women, as.numeric(fra_women$est_endpoint_d) - 365)
fra_shape_women <- as.numeric(fra_women$mle_shape)
fra_scale_women <- as.numeric(fra_women$mle_scale)
fra_agedays_women <- as.numeric(sort(data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"), ]$AGEDAYS))
fra_surv_women <- gpd_survival(fra_shape_women, fra_scale_women, fra_thresh_women, fra_agedays_women)
fra_km_women <- survfit(Surv(fra_agedays_women) ~ 1)
fra_women_data <- data.frame(data = fra_agedays_women, time = fra_agedays_women)
fra_women_data$status <- 1
fra_women_na <- nelsonaalen(data = fra_women_data, timevar = time, statusvar = status)
fra_women_gh <- gpd_cum_hazard(fra_shape_women, fra_scale_women, fra_thresh_women, fra_agedays_women)
fra_women_est_endpoint <- as.numeric(fra_women$est_endpoint_d)

fra_men <- estimators_france[which(estimators_france$SEX == "M" & estimators_france$DDATE == "2015"), ]
fra_thresh_men <- as.numeric(fra_men$THRESHOLD)
fra_oldest_men <- as.numeric(fra_men$OLDEST)
fra_obs_men <- seq(fra_thresh_men, as.numeric(fra_men$est_endpoint_d) - 365)
fra_shape_men <- as.numeric(fra_men$mle_shape)
fra_scale_men <- as.numeric(fra_men$mle_scale)
fra_men <- estimators_france[which(estimators_france$SEX == "M" & estimators_france$DDATE == "2015"), ]
fra_thresh_men <- as.numeric(fra_men$THRESHOLD)
fra_oldest_men <- as.numeric(fra_men$OLDEST)
fra_obs_men <- seq(fra_thresh_men, as.numeric(fra_men$est_endpoint_d) - 365)
fra_shape_men <- as.numeric(fra_men$mle_shape)
fra_scale_men <- as.numeric(fra_men$mle_scale)
fra_agedays_men <- as.numeric(sort(data_france[which(data_france$DDATE == "2015" & data_france$SEX == "M"), ]$AGEDAYS))
fra_surv_men <- gpd_survival(fra_shape_men, fra_scale_men, fra_thresh_men, fra_agedays_men)
fra_km_men <- survfit(Surv(fra_agedays_men) ~ 1)
fra_men_est_endpoint <- as.numeric(fra_men$est_endpoint_d)

fra_haz_men <- gpd_hazard(fra_shape_men, fra_scale_men, fra_thresh_men, fra_obs_men)
fra_haz_einmahl_men <- einmahl_direct_hazard(fra_shape_men, as.numeric(fra_men$est_endpoint_d), fra_obs_men)
fra_m <- data_france[which(data_france$DDATE == "2015" & data_france$SEX == "M"), ]
ad_fra_m <- sort(fra_m$AGEDAYS)
u_fra_m <- seq(min(ad_fra_m), as.numeric(fra_men$est_endpoint_d), length.out = 100)
fra_mu_men <- muhaz(ad_fra_m, bw.method = "g", min.time = fra_thresh_men, max.time = as.numeric(fra_men$est_endpoint_d) - 365)
fra_men_data <- data.frame(data = fra_agedays_men, time = fra_agedays_men)
fra_men_data$status <- 1
fra_men_na <- nelsonaalen(data = fra_men_data, timevar = time, statusvar = status)
fra_men_gh <- gpd_cum_hazard(fra_shape_men, fra_scale_men, fra_thresh_men, fra_agedays_men)