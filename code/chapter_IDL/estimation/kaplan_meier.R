source("../../helper/package_check.R")
source("../../helper/data.R")
source('./estimators.R')
source('./parametric_gpd.R')
load_idl_complete()
load_estimations()

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

ger_women <- estimators_germany[which(estimators_germany$SEX == "F" & estimators_germany$DDATE == "2001"), ]
ger_thresh_women <- as.numeric(ger_women$THRESHOLD)
ger_oldest_women <- as.numeric(ger_women$OLDEST)
ger_obs_women <- seq(ger_thresh_women, as.numeric(ger_women$est_endpoint_d) - 365)
ger_shape_women <- as.numeric(ger_women$mle_shape)
ger_scale_women <- as.numeric(ger_women$mle_scale)

ger_men <- estimators_germany[which(estimators_germany$SEX == "M" & estimators_germany$DDATE == "2000"), ]
ger_thresh_men <- as.numeric(ger_men$THRESHOLD)
ger_oldest_men <- as.numeric(ger_men$OLDEST)
ger_obs_men <- seq(ger_thresh_men, as.numeric(ger_men$est_endpoint_d) - 365)
ger_shape_men <- as.numeric(ger_men$mle_shape)
ger_scale_men <- as.numeric(ger_men$mle_scale)

pdf("./plots/kaplan_meier/kaplan-meier-survival_germany.pdf", width =14 , height = 8.6)
par(mfrow = c(1, 2))
ger_agedays_women <- as.numeric(sort(data_germany[which(data_germany$DDATE == "2001" & data_germany$SEX == "F"), ]$AGEDAYS))
ger_surv_women <- gpd_survival(ger_shape_women, ger_scale_women, ger_thresh_women, ger_agedays_women)
ger_km_women <- survfit(Surv(ger_agedays_women) ~ 1)
plot(ger_km_women, xlim = c(min(ger_agedays_women), max(ger_agedays_women)), main = "German Women, 2001", xlab = "Days Lived", ylab = "Estimated Survival Function", col = "blue")
lines(ger_agedays_women, ger_surv_women, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
ger_agedays_men <- as.numeric(sort(data_germany[which(data_germany$DDATE == "2000" & data_germany$SEX == "M"), ]$AGEDAYS))
ger_surv_men <- gpd_survival(ger_shape_men, ger_scale_men, ger_thresh_men, ger_agedays_men)
ger_km_men <- survfit(Surv(ger_agedays_men) ~ 1)
plot(ger_km_men, xlim = c(min(ger_agedays_men), max(ger_agedays_men)), main = "German Men, 2000", xlab = "Days Lived", ylab = "", col = "blue")
lines(ger_agedays_men, ger_surv_men, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
dev.off()

fra_women <- estimators_france[which(estimators_france$SEX == "F" & estimators_france$DDATE == "2017"), ]
fra_thresh_women <- as.numeric(fra_women$THRESHOLD)
fra_oldest_women <- as.numeric(fra_women$OLDEST)
fra_obs_women <- seq(fra_thresh_women, as.numeric(fra_women$est_endpoint_d) - 365)
fra_shape_women <- as.numeric(fra_women$mle_shape)
fra_scale_women <- as.numeric(fra_women$mle_scale)

fra_men <- estimators_france[which(estimators_france$SEX == "M" & estimators_france$DDATE == "2015"), ]
fra_thresh_men <- as.numeric(fra_men$THRESHOLD)
fra_oldest_men <- as.numeric(fra_men$OLDEST)
fra_obs_men <- seq(fra_thresh_men, as.numeric(fra_men$est_endpoint_d) - 365)
fra_shape_men <- as.numeric(fra_men$mle_shape)
fra_scale_men <- as.numeric(fra_men$mle_scale)

pdf("./plots/kaplan_meier/kaplan-meier-survival_france.pdf", width = 14, height = 8.6)
par(mfrow = c(1, 2))
fra_agedays_women <- as.numeric(sort(data_france[which(data_france$DDATE == "2017" & data_france$SEX == "F"), ]$AGEDAYS))
fra_surv_women <- gpd_survival(fra_shape_women, fra_scale_women, fra_thresh_women, fra_agedays_women)
fra_km_women <- survfit(Surv(fra_agedays_women) ~ 1)
plot(fra_km_women, xlim = c(min(fra_agedays_women), max(fra_agedays_women)), main = "French Women, 2017", xlab = "Days Lived", ylab = "Estimated Survival Function", col = "blue")
lines(fra_agedays_women, fra_surv_women, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
fra_agedays_men <- as.numeric(sort(data_france[which(data_france$DDATE == "2015" & data_france$SEX == "M"), ]$AGEDAYS))
fra_surv_men <- gpd_survival(fra_shape_men, fra_scale_men, fra_thresh_men, fra_agedays_men)
fra_km_men <- survfit(Surv(fra_agedays_men) ~ 1)
plot(fra_km_men, xlim = c(min(fra_agedays_men), max(fra_agedays_men)), main = "French Men, 2015", xlab = "Days Lived", ylab = "", col = "blue")
lines(fra_agedays_men, fra_surv_men, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
dev.off()
