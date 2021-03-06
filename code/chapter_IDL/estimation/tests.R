source("../../helper/package_check.R")
source("../../helper/data.R")
source("./estimators.R")
source("./parametric_gpd.R")
source("./necessary_variables.R")

tests <- data.frame(
  negative_index_france = double(),
  negative_index_germany = double(),
  finite_endpoint_france = double(),
  finite_endpoint_germany = double()
)

negative_index <- function(v_mle_shape, v_nobs) {
  return(sqrt(min(v_nobs)) * max(v_mle_shape))
}

finite_endpoint <- function(v_mle_shape, v_mle_scale, v_est_endpoint, v_nobs) {
  n <- length(v_nobs)
  k <- min(v_nobs)
  variance <- function(mle_shape) {
    return(
      2 + 2 * mle_shape^(-1) + 5 * mle_shape^(-2) + 4 * mle_shape^(-3) +
        mle_shape^(-4)
    )
  }
  d <- 0
  for (it in seq(1, n)) {
    d <- d + k / (v_mle_scale[it] * variance(v_mle_shape[it]))^2
  }
  omega_tilde <- 0
  for (it in seq(1, n)) {
    omega_tilde <- omega_tilde + k / (v_mle_scale[it] *
      variance(v_mle_shape[it]))^2 * 1 / d * v_est_endpoint[it]
  }
  T <- 0
  for (it in seq(1, n)) {
    T <- T + k / (v_mle_scale[it] * variance(v_mle_shape[it]))^2 *
      (v_est_endpoint[it] - omega_tilde)^2
  }
  return(T)
}

estimators_germany <- estimators_germany %>% drop_na()

ger_women <- estimators_germany[estimators_germany$SEX == "F", ]
ger_women_nobs <- as.numeric(ger_women$N_OBSERVATIONS)
ger_shape_women <- as.numeric(ger_women$mle_shape)
ger_scale_women <- as.numeric(ger_women$mle_scale)
ger_women_est_endpoint <- as.numeric(ger_women$est_endpoint_d)
ger_neg_test_women <- negative_index(ger_shape_women, ger_women_nobs)
ger_finite_endp_women <-
  finite_endpoint(
    ger_shape_women,
    ger_scale_women,
    ger_women_est_endpoint,
    ger_women_nobs
  )


ger_men <- estimators_germany[estimators_germany$SEX == "M", ]
ger_men_nobs <- as.numeric(ger_men$N_OBSERVATIONS)
ger_shape_men <- as.numeric(ger_men$mle_shape)
ger_scale_men <- as.numeric(ger_men$mle_scale)
ger_women_est_endpoint <- as.numeric(ger_men$est_endpoint_d)
ger_neg_test_men <- negative_index(ger_shape_men, ger_men_nobs)
ger_finite_endp_men <-
  finite_endpoint(
    ger_shape_men,
    ger_scale_men,
    ger_men_est_endpoint,
    ger_men_nobs
  )

estimators_france <- estimators_france %>% drop_na()
fra_women <- estimators_france[estimators_france$SEX == "F", ]
fra_women_nobs <- as.numeric(fra_women$N_OBSERVATIONS)
fra_shape_women <- as.numeric(fra_women$mle_shape)
fra_scale_women <- as.numeric(fra_women$mle_scale)
fra_women_est_endpoint <- as.numeric(fra_women$est_endpoint_d)
fra_neg_test_women <- negative_index(fra_shape_women, fra_women_nobs)
fra_finite_endp_women <-
  finite_endpoint(
    fra_shape_women,
    fra_scale_women,
    fra_women_est_endpoint,
    fra_women_nobs
  )

fra_men <- estimators_france[estimators_france$SEX == "M", ]
fra_men_nobs <- as.numeric(fra_men$N_OBSERVATIONS)
fra_men_nobs <- as.numeric(fra_men$N_OBSERVATIONS)
fra_shape_men <- as.numeric(fra_men$mle_shape)
fra_scale_men <- as.numeric(fra_men$mle_scale)
fra_men_est_endpoint <- as.numeric(fra_men$est_endpoint_d)
fra_neg_test_men <- negative_index(fra_shape_men, fra_men_nobs)
fra_finite_endp_men <-
  finite_endpoint(
    fra_shape_men,
    fra_scale_men,
    fra_men_est_endpoint,
    fra_men_nobs
  )