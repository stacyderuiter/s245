#' Estimate overdispersion for count data models
#'
#' From notes by Ben Bolker at https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor
#'
#' @param model a fitted model object
#' @export
#' @return The estimated overdispersion factor for the model.
#'
#'
#'

overdisp_fun <- function(model) {
  rdf <- stats::df.residual(model)
  rp <- stats::residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- stats::pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  return(prat)
}
