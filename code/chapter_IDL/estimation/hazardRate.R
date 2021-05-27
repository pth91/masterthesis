source("./code/package_check.R")
source("./code/data/data.R")
library("writexl")
library("survival")
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

ger_women <- data_germany[data_germany$SEX == "F", ]
# for german women 2001 has the most observations
ger_women_agedays <- ger_women[ger_women$DDATE == "2001", ]
ger_women_agedays <- ger_women_agedays$AGEDAYS
# kaplan meier needs ordred data
ger_women_agedays <- as.numeric(sort(ger_women_agedays))
ger_women_fit <- survfit(Surv(ger_women_agedays) ~ 1)


ger_men <- data_germany[data_germany$SEX == "M", ]
# for german men 2000 has the most observations
ger_men_agedays <- ger_men[ger_men$DDATE == "2000", ]
ger_men_agedays <- as.numeric(ger_men_agedays$AGEDAYS)
ger_men_agedays <- sort(ger_men_agedays)
ger_men_fit <- survfit(Surv(ger_men_agedays) ~ 1)

fra_women <- data_france[data_france$SEX == "F", ]
# for french women 2017 has the most observations; n = 686
fra_women_agedays <- fra_women[fra_women$DDATE == "2017", ]
fra_women_agedays <- fra_women_agedays$AGEDAYS
# kaplan meier needs ordred data
fra_women_agedays <- as.numeric(sort(fra_women_agedays))
fra_women_fit <- survfit(Surv(fra_women_agedays) ~ 1)


fra_men <- data_france[data_france$SEX == "M", ]
# for french men 2015 has the most observations; n = 73
fra_men_agedays <- fra_men[fra_men$DDATE == "2015", ]
fra_men_agedays <- as.numeric(fra_men_agedays$AGEDAYS)
fra_men_agedyas <- sort(fra_men_agedays)
fra_men_fit <- survfit(Surv(fra_men_agedays) ~ 1)

png("./code/estimation/plots/kaplan-meier.png", width = 1920, height = 1080)
par(mfrow = c(2, 2))
plot(ger_women_fit,
  main = "kaplan-meier for german women in 2001", xlab = "time",
  ylab = "survival function", xlim = c(min(ger_women_agedays), max(ger_women_agedays))
)
plot(ger_men_fit,
  main = "kaplan-meier for german men in 2000", xlab = "time",
  ylab = "survival function", xlim = c(min(ger_men_agedays), max(ger_men_agedays))
)
plot(fra_women_fit,
  main = "kaplan-meier for french women in 2017", xlab = "time",
  ylab = "survival function", xlim = c(min(fra_women_agedays), max(fra_women_agedays))
)
plot(fra_men_fit,
  main = "kaplan-meier for french men in 2015", xlab = "time",
  ylab = "survival function", xlim = c(min(fra_men_agedays), max(fra_men_agedays))
)
dev.off()

# cumulative hazard function H = exp{-H(t)} can be obtained with H = -log(S(T))
# and S is the kaplan-meier estimator
ger_women_H <- -log(summary(ger_women_fit)$surv)
ger_men_H <- -log(summary(ger_men_fit)$surv)
fra_women_H <- -log(summary(fra_women_fit)$surv)
fra_men_H <- -log(summary(fra_men_fit)$surv)

png("./code/estimation/plots/cumulative_hazard.png", width = 1920, height = 1080)
par(mfrow = c(2, 2))
plot(ger_women_H, main = "cumulative Hazard by kaplan-meier, women germany")
plot(ger_men_H, main = "cumulative Hazard by kaplan-meier, men germany")
plot(fra_women_H, main = "cumulative Hazard by kaplan-meier, women france")
plot(fra_men_H, main = "cumulative Hazard by kaplan-meier, men france")
dev.off()
