#' carry out parametric bootstrap to get CIs for model predictions
#'
#' @param model a fitted model object created by lm() or glm() or geeglm()
#' @param new_data data frame with data for which to make predictions and generate bootstrap CIs
#' @param predictor the covariate for which to make predictions. other predictors in the model will be held constant at their median value, or the most commonly observed value in the dataset.
#' @param nboot number of bootstrap iterations. Defaults to 1000. Ignored if boot is FALSE.
#' @param conf_level confidence level as a proportion, default is 0.95 for 95 percent confidence
#'
#' @export
#' @return a data.frame with variables CIlower and CIupper, the lower and upper bounds of the parametric bootstrap CI

para_boot <- function(model, new_data, predictor, nboot, conf_level=0.95){

#get the "design(ish) matrix"
coefs <- stats::coef(model)
f <- as.character(stats::formula(model))
f <- paste("~", f[3], sep="")
#adjust for other factor predictors (other than the one being plotted)
# faci <- which(unlist(lapply(new_data, function(x) class(x) %in% c('factor', 'character'))))
# faci <- faci[names(faci) != predictor]
# xrows <- 0
# for (v in c(1:length(faci))){
  # add_new_data <- data.frame(xv = levels(new_data[,faci[v]]))
  # names(add_new_data) <- names(new_data)[faci[v]]
  # add_new_data[,2:ncol(new_data)] <- new_data[1,names(new_data)!=names(new_data)[faci[v]]]
  # xrows <- xrows + nrow(add_new_data)
# }
xmat <- stats::model.matrix(stats::as.formula(f), new_data)
# xmat <- xmat[1:(nrow(xmat)-xrows),]

#parametric bootstrap to get CIs on the mean factor level effects
#Assume (this is reasonable given our model) that the
#parameter estimates
#follow a multivariate normal distribution with means
#equal to the parameter estimates from the fitted model and
#variance-covariance matrix also from the fitted model
#(accessed via summary(model)$cov.scaled)
# Use rmvnorm (from mvnorm library) to sample coefs from this
#MVN (say maybe 5000 iterations) and multiply by the design matrix
#to get predictions (make sure the 2 matrices are the right shapes
#for matrix multiplication... ).  values should be a matrix with
#nrows=number of parameters (factor combination levels) and
#ncol=number of iterations). get the mvn values that will be the
#bootstrap "output" for each iteration

boot_coefs <- t(mvtnorm::rmvnorm(nboot, mean=model$coef,
                        sigma=summary(model)$cov.scaled,
                        method="svd"))
boot_CI <- list()
#now multiply the prediction-data
#by the coefficients  (with the appropriate link function! to get
#predicted value in each condition for each #iteration:
if (model$family$link == "logit"){
    boot_pred <- mosaic::ilogit(xmat %*% boot_coefs)
}
if (model$family$link == "identity"){
    boot_pred <- xmat %*% boot_coefs
}
if (model$family$link == "log"){
    boot_pred <- exp(xmat %*% boot_coefs)
}
if (model$family$link == 'probit'){
  boot_pred <- VGAM::probit(xmat %*% boot_coefs, inverse=TRUE)
}
if (model$family$link == 'cloglog'){
  boot_pred <- VGAM::cloglog(xmat %*% boot_coefs, inverse=TRUE)
}
if (model$family$link == 'inverse'){
  boot_pred <- 1/(xmat %*% boot_coefs)
}
# then to turn that into a CI, find percentile based CIs for
#each row (each row is one combo of factor levels).
boot_CI <- data.frame(upper=apply(boot_pred, 1,
                                  stats::quantile,
                                  probs= 1 - ((1-conf_level)/2)))
boot_CI$lower <- apply(boot_pred, 1, stats::quantile,
                       probs= (1-conf_level)/2 )
return(boot_CI)
}
