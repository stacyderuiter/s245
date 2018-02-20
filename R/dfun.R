#' function to extract the overdispersion parameter from a quasi model
#'
#' @param object a fitted model object
#'
#' @export
#' @return dispersion parameter estimate
#'
#'
#'

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}
