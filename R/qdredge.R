#' dredge for quasi-poisson models
#'
#' @param model a fitted model object created by glm() with family=quasipoisson
#' @param family family to which to update family of the model object. Default is 'x.quasipoisson' - this uses the AIC score from the Poisson version of the quasi-Poisson model, keeping the rest of the quasi-Poisson model object.
#' @param na.action default is 'na.fail', for compatibility with dredge()
#' @param chat c-hat parameter passed to the QAIC function from package MuMIn This is the variance inflation factor, here estimated using the dispersion parameter from the quasi-poisson model.
#' @param rank Function used to rank models; default is 'QAIC'
#' @param ... Additional arguments to be passed to dredge()
#'
#' @export
#' @return A dredge model selection object. See help for dredge() in package MuMIn for details.
#'
#'
#'
qdredge <- function(model, family='x.quasipoisson',
                    na.action=na.fail,
                    rank='QAIC', chat = dfun(model), ...){
  model2 <- stats::update(model, family=family, na.action=na.action)
  (dt <- MuMIn::dredge(model2, rank=rank, chat=chat, ...))
}
