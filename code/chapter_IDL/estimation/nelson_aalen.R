source("../../helper/package_check.R")
source("../../helper/data.R")
source("./estimators.R")
source("./parametric_gpd.R")
source("./necessary_variables.R")

pdf("./figures/nelson_aalen/nelson-aalen_germany.pdf", width = 14, height = 8.6)
par(mfrow = c(1, 2))
plot(
    ger_agedays_women, ger_women_na, "l",
    ylim = range(ger_women_na, ger_women_gh),
    main = "German Women, 2001",
    xlab = "Days Lived", ylab = "Estimated Cumulated Hazard Rate",
    col = "blue"
)
lines(x = ger_agedays_women, y = ger_women_gh, col = "red")
lines(unique(ad_ger_w), -log(ger_km_women$surv), col = "green")
legend(
    "topleft",
    c("Nelson-Aalen", "Indirect GPD", "Direct GPD"),
    fill = c("blue", "green", "red")
)
plot(
    ger_agedays_men, ger_men_na, "l",
    ylim = range(ger_men_na, ger_men_gh),
    main = "German Men, 2000",
    xlab = "Days Lived", ylab = "",
    col = "blue"
)
lines(x = ger_agedays_men, y = ger_men_gh, col = "red")
lines(unique(ad_ger_m), -log(ger_km_men$surv), col = "green")
legend(
    "topleft",
    c("Nelson-Aalen", "Indirect GPD", "Direct GPD"),
    fill = c("blue", "green", "red")
)
dev.off()



pdf("./figures/nelson_aalen/nelson-aalen_france.pdf", width = 14, height = 8.6)
par(mfrow = c(1, 2))
plot(
    fra_agedays_women, fra_women_na, "l",
    ylim = range(fra_women_na, fra_women_gh),
    main = "French Women, 2017",
    xlab = "Days Lived", ylab = "Estimated Cumulated Hazard Rate",
    col = "blue"
)
lines(x = fra_agedays_women, y = fra_women_gh, col = "red")
lines(unique(ad_fra_w), -log(fra_km_women$surv), col = "green")
legend(
    "topleft",
    c("Nelson-Aalen", "Indirect GPD", "Direct GPD"),
    fill = c("blue", "green", "red")
)
plot(
    fra_agedays_men, fra_men_na, "l",
    ylim = range(fra_men_na, fra_men_gh),
    main = "French Men, 2015",
    xlab = "Days Lived", ylab = "",
    col = "blue"
)
lines(x = fra_agedays_men, y = fra_men_gh, col = "red")
lines(unique(ad_fra_m), -log(fra_km_men$surv), col = "green")
legend(
    "topleft",
    c("Nelson-Aalen", "Indirect GPD", "Direct GPD"),
    fill = c("blue", "green", "red")
)
dev.off()