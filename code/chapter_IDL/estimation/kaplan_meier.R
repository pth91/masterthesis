source("../../helper/package_check.R")
source("../../helper/data.R")
source("./estimators.R")
source("./parametric_gpd.R")
source("./necessary_variables.R")

pdf(
    "./figures/kaplan_meier/kaplan-meier-survival_germany.pdf",
    width = 14, height = 8.6
)
par(mfrow = c(1, 2))
plot(ger_km_women,
    xlim = c(min(ger_agedays_women), max(ger_agedays_women)),
    main = "German Women, 2001",
    xlab = "Days Lived", ylab = "Estimated Survival Function",
    col = "blue"
)
lines(ger_agedays_women, ger_surv_women, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
plot(
    ger_km_men,
    xlim = c(min(ger_agedays_men), max(ger_agedays_men)),
    main = "German Men, 2000",
    xlab = "Days Lived", ylab = "",
    col = "blue"
)
lines(ger_agedays_men, ger_surv_men, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
dev.off()


pdf(
    "./figures/kaplan_meier/kaplan-meier-survival_france.pdf",
    width = 14, height = 8.6
)
par(mfrow = c(1, 2))
plot(fra_km_women,
    xlim = c(min(fra_agedays_women), max(fra_agedays_women)),
    main = "French Women, 2017",
    xlab = "Days Lived", ylab = "Estimated Survival Function",
    col = "blue"
)
lines(fra_agedays_women, fra_surv_women, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
plot(
    fra_km_men,
    xlim = c(min(fra_agedays_men), max(fra_agedays_men)),
    main = "French Men, 2015",
    xlab = "Days Lived", ylab = "",
    col = "blue"
)
lines(fra_agedays_men, fra_surv_men, col = "red")
legend("topright", c("Kaplan-Meier", "Direct GPD"), fill = c("blue", "red"))
dev.off()