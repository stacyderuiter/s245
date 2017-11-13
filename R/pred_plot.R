#' make prediction plots for lm and glm objects using ggformula.
#'
#' @param model a fitted model object created by lm() or glm() or geeglm()
#' @param predictor the covariate for which to make predictions. other predictors in the model will be held constant at their median value, or the most commonly observed value in the dataset.
#' @param xlab X axis label for plot (defaults to name of predictor variable)
#' @param ylab Y axis label for plot (defaults to "Predictions from Fitted Model")
#' @param conf_int logical: should confidence intervals be shown (as error bars or confidence band)?
#' @param conf_level confidence level as a proportion, default is 0.95 for 95 percent confidence
#' @param boot logical: should CIs be derived via a parametric bootstrap? Defaults to FALSE except if the model is a GEE, then defaults to TRUE.
#' @param nboot number of bootstrap iterations. Defaults to 1000. Ignored if boot is FALSE.
#' @param new_data_out logical: should data set used for predictions be output? If TRUE, result is a list including the plot object and the data frame.
#' @param ... Additional arguments to be passed to plotting function
#'
#' @export
#' @return A ggplot2 plot created using ggformula gf_line or gf_point


pred_plot <- function(model, predictor, xlab=NULL, ylab=NULL,
                      conf_level=0.95, conf_int=TRUE,
                      boot=FALSE, nboot=1000, new_data_out=FALSE,
                      ...){
  if ('gee' %in% class(model)){
    boot=TRUE
  }
  fixed_vals <- get_fixed(model)
  data <- model$model
  #make dataset for predictions
  if (class(data[,predictor]) %in% c('factor', 'character')){
    new_data <- data.frame(x=levels(factor(data[,predictor])))
  }else{
    new_data <- data.frame(x=seq(from=min(data[,predictor], na.rm=TRUE),
                               to = max(data[,predictor], na.rm=TRUE),
                               length.out=250))
  }
  xi <- which(names(fixed_vals)==predictor)
  new_data[,c(2:(ncol(data)-1))] <- fixed_vals[,-xi]
  if (ncol(new_data) > 1){
  names(new_data)[2:ncol(fixed_vals)] <- names(fixed_vals)[-xi]
  }
  names(new_data)[1] <- predictor
  if (!boot){
    #make predictions
    if (conf_int){
      pred <- predict(model, newdata=new_data, type='response', se.fit=TRUE)
      new_data$preds <- pred$fit
      zstar <- qnorm((1-conf_level)/2, lower.tail=FALSE)
      new_data$CIl <- pred$fit - zstar*pred$se.fit
      new_data$CIu <- pred$fit + zstar*pred$se.fit
    }else{#no CIs
      pred <- predict(model, newdata=new_data, type='response')
      new_data$preds <- pred
      }
  }else{#bootstrap CIs
    pred <- predict(model, newdata=new_data, type='response')
    new_data$preds <- pred
    if (conf_int){
      boot_CI <- para_boot(model, new_data, predictor,
                           nboot, conf_level)
      new_data$CIl <- boot_CI$lower
      new_data$CIu <- boot_CI$upper
    }
    }

  if (is.null(xlab)){
    xlab = predictor
  }
  if (is.null(ylab)){
    ylab = 'Predictions from Fitted Model'
  }
  form <- as.formula(paste("preds ~ ", predictor))
  form2 <- as.formula(paste("CIl + CIu ~ ", predictor))

  #make plot if categorical predictor
  if (class(data[,predictor]) %in% c('factor', 'character')){
  if (conf_int){
  P <- ggformula::gf_point(form, data=new_data) %>%
    ggformula::gf_labs(x=xlab, y=ylab) %>%
    ggformula::gf_errorbar(form2, data=new_data)
  }else{
  P <- ggformula::gf_point(form, data=new_data) %>%
    ggformula::gf_labs(x=xlab, y=ylab)
  }
  }else{#make plot if quant predictor
  if (conf_int){
  P <- ggformula::gf_line(form, data=new_data) %>%
    ggformula::gf_labs(x=xlab, y = ylab) %>%
    ggformula::gf_ribbon(form2, data=new_data)
  }else{
   P <- ggformula::gf_line(form, data=new_data) %>%
     ggformula::gf_labs(x=xlab, y = ylab)
  }
  }
  P
  if (new_data_out){
    return(list(plot=P, new_data=new_data))
  }else{
    return(P)
  }
}

