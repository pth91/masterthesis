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