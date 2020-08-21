#' helper function for qdredge, not normally used directly by users. Modify a glm() output object so that it contains a quasipoisson fit but the AIC (likelihood) from the equivalent regular Poisson model
#'
#' @param ... Additional arguments
#'
#' @export
#' @return A fitted model object
#'
#'


# modify a glm() output object so that
# it contains a quasipoisson fit but the
# AIC (likelihood) from the equivalent regular Poisson model
x.quasipoisson <- function(...) {
  res <- stats::quasipoisson(...)
  res$aic <- stats::poisson(...)$aic
  res
}

