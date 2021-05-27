source("./code/package_check.R")
source("./code/data/data.R")
library("writexl")
load_idl_complete()

data_germany <- idl_complete[idl_complete$DCOUNTRY == "DEU", ]
data_france <- idl_complete[idl_complete$DCOUNTRY == "FRA", ]

data_germany$DDATE <- substr(data_germany$DDATE, 7, 10)
data_france$DDATE <- substr(data_france$DDATE, 7, 10)

death_years_germany <- sort(unique(data_germany$DDATE))
death_years_france <- sort(unique(data_france$DDATE))

ger_women <- data_germany[data_germany$SEX == "F", ]
# for german women 2001 has the most observations
ger_women_agedays <- ger_women[ger_women$DDATE == "2001", ]
ger_women_agedays <- ger_women_agedays$AGEDAYS
# kaplan meier needs ordred data
ger_women_agedays <- as.numeric(sort(ger_women_agedays))
ger_women_fit <- fpot(ger_women_agedays, threshold=min(ger_women_agedays))
ger_women_params <- ger_women_fit$param


ger_men <- data_germany[data_germany$SEX == "M", ]
# for german men 2000 has the most observations
ger_men_agedays <- ger_men[ger_men$DDATE == "2000", ]
ger_men_agedays <- as.numeric(ger_men_agedays$AGEDAYS)
ger_men_agedays <- sort(ger_men_agedays)
ger_men_fit <- fpot(ger_men_agedays, threshold=min(ger_men_agedays))
ger_men_params <- ger_men_fit$param

fra_women <- data_france[data_france$SEX == "F", ]
# for french women 2017 has the most observations; n = 686
fra_women_agedays <- fra_women[fra_women$DDATE == "2017", ]
fra_women_agedays <- fra_women_agedays$AGEDAYS
# kaplan meier needs ordred data
fra_women_agedays <- as.numeric(sort(fra_women_agedays))
fra_women_fit <- fpot(fra_women_agedays, threshold=min(fra_women_agedays))
fra_women_params <- fra_women_fit$param


fra_men <- data_france[data_france$SEX == "M", ]
# for french men 2015 has the most observations; n = 73
fra_men_agedays <- fra_men[fra_men$DDATE == "2015", ]
fra_men_agedays <- as.numeric(fra_men_agedays$AGEDAYS)
fra_men_agedyas <- sort(fra_men_agedays)
fra_men_fit <- fpot(fra_men_agedays, threshold=min(fra_men_agedays))
fra_men_params <- fra_men_fit$param

hazard_rate <- function(t, threshold, scale, gamma){
    return(1 / (1 + gamma * (t - threshold)/scale))
}

png("./code/estimation/plots/hazard_rate_direct_estimation.png", width=1920, height=1080)
par(mfrow = c(2, 2))
plot(
    ger_women_agedays, hazard_rate(ger_women_agedays, min(ger_women_agedays), ger_women_params[1], ger_women_params[2]),
    xlab = "Days Lived", ylab = "h(x)", main = "German Women, 2001", xaxt="n"
)
axis(1, at = c(min(ger_women_agedays), max(ger_women_agedays)))
plot(
    ger_men_agedays, hazard_rate(ger_men_agedays, min(ger_men_agedays), ger_men_params[1], ger_men_params[2]),
    xlab = "Days Lived", ylab = "h(x)", main = "German Men, 2000"
)
axis(1, at = c(min(ger_men_agedays), max(ger_men_agedays)))
plot(
    fra_women_agedays, hazard_rate(fra_women_agedays, min(fra_women_agedays), fra_women_params[1], fra_women_params[2]),
    xlab = "Days Lived", ylab = "h(x)", main = "French Women, 2017"
)
axis(1, at = c(min(fra_women_agedays), max(fra_women_agedays)))
plot(
    fra_men_agedays, hazard_rate(fra_men_agedays, min(fra_men_agedays), fra_men_params[1], fra_men_params[2]),
    xlab = "Days Lived", ylab = "h(x)", main = "French Men, 2015"
)
axis(1, at = c(min(fra_men_agedays), max(fra_men_agedays)))
dev.off()
