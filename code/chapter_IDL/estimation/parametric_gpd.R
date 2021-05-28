gpd_hazard <- function(mle_shape, mle_scale, threshold, x) {
  return(1 / (mle_scale + mle_shape * (x - threshold)))
}

gpd_cum_hazard <- function(mle_shape, mle_scale, threshold, x) {
  return(
    (1 / mle_shape) *
      (log(
        mle_scale + mle_shape * (x - threshold)
      )
      - log(mle_scale))
  )
}

gpd_survival <- function(mle_shape, mle_scale, threshold, x) {
  return((1 + mle_shape * (x - threshold) / mle_scale)^(-1 / mle_shape))
}

mean_res_life <- function(mle_shape, mle_scale, threshold, x) {
  nominator <- -(mle_scale^(1 / mle_shape)) / (mle_shape - 1) *
    (mle_scale + mle_shape * (x - threshold))^(1 - 1 / mle_shape)

  denominator <- gpd_survival(mle_shape, mle_scale, threshold, x)
  return(nominator / denominator)
}