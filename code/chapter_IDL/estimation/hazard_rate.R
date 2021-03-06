source("../../helper/package_check.R")
source("../../helper/data.R")
source("./necessary_variables.R")
source("./estimators.R")
source("./parametric_gpd.R")

pdf("./figures/hazard_rate/hazard_rate_germany.pdf", width = 14, height = 8.6)
plot(ger_obs_women, ger_haz_women,
    main = "German Women, 2001",
    xlab = "Days Lived", ylab = "Estimated Hazrd Rate", "l"
)
lines(ger_obs_women, ger_haz_einmahl_women, col = "red")
lines(ger_mu_women$est.grid, ger_mu_women$haz.est, col = "blue")
abline(v = ger_oldest_women, col = "yellow")
abline(v = ad_ger_w[length(ad_ger_w) - 1], col = "green")
abline(v = ad_ger_w[length(ad_ger_w) - 2], col = "grey")
legend(
    "topleft",
    c("Kernel Estimator", "Direct GPD"),
    fill = c("blue", "red")
)
dev.off()
pdf("./figures/hazard_rate/hazard_rate_france.pdf", width = 14, height = 8.6)
par(mfrow = c(1, 2))
plot(fra_obs_women, fra_haz_women,
    main = "French Women, 2017",
    xlab = "Days Lived", ylab = "Estimated Hazard Rate", "l"
)
lines(fra_obs_women, fra_haz_einmahl_women, col = "red")
lines(fra_mu_women$est.grid, fra_mu_women$haz.est, col = "blue")
abline(v = fra_oldest_women, col = "yellow")
abline(v = ad_fra_w[length(ad_fra_w) - 1], col = "green")
abline(v = ad_fra_w[length(ad_fra_w) - 2], col = "grey")
legend(
    "topleft",
    c("Kernel Estimator", "Direct GPD"),
    fill = c("blue", "red")
)
plot(fra_obs_men, fra_haz_men,
    main = "French Men, 2015",
    xlab = "Days Lived", ylab = "", "l"
)
lines(fra_obs_men, fra_haz_einmahl_men, col = "red")
lines(fra_mu_men$est.grid, fra_mu_men$haz.est, col = "blue")
abline(v = fra_oldest_men, col = "yellow")
abline(v = ad_fra_m[length(ad_fra_m) - 1], col = "green")
abline(v = ad_fra_m[length(ad_fra_m) - 2], col = "grey")
legend(
    "topleft",
    c("Kernel Estimator", "Direct GPD"),
    fill = c("blue", "red")
)
dev.off()