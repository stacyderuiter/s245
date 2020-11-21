#' make prediction plots for lm and glm objects using ggformula.
#'
#' @param model a fitted model object created by lm() or glm() or geeglm() or glmmTMB() or model.avg()
#' @param predictor the covariate for which to make predictions. other predictors in the model will be held constant at their median value, or the most commonly observed value in the dataset.
#' @param data The dataset to which the model was fitted. Only required (and only used) if the model input is an "averaging" object from model.avg().
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
#' @return A ggplot2 plot created using ggformul::gf_line() or gf_point()


pred_plot <- function(model, predictor, data = NULL,
                      xlab = NULL, ylab = NULL,
                      conf_level = 0.95, conf_int = TRUE,
                      boot = FALSE, nboot = 1000, new_data_out = FALSE,
                      ...){
  if ('gee' %in% class(model)){
    boot=TRUE
  }

  if ("averaging" %in% class(model)){
    if (is.null(data)){
      stop('pred_plot: input "data" is required, if input model is from model.avg().')
    }else{
      new_data <- get_new_data(data = data, predictor=predictor,
                   fixed_vals=get_fixed(data))
    }
  }else{
    new_data <- get_new_data(data=model, predictor=predictor,
                           fixed_vals=get_fixed(model))
  }

  if (!boot){
    #make predictions
    if (conf_int){
      pred <- stats::predict(model,
                             newdata=new_data,
                             type='link',
                             se.fit=TRUE)
      ilink <- gratia::inv_link(model)
      new_data$preds <- ilink(pred$fit)
      zstar <- stats::qnorm((1-conf_level)/2, lower.tail=FALSE)
      new_data$CIl <- ilink(pred$fit - zstar*pred$se.fit)
      new_data$CIu <- ilink(pred$fit + zstar*pred$se.fit)
    }else{#no CIs
      pred <- stats::predict(model,
                             newdata=new_data,
                             type='response')
      new_data$preds <- pred
      }
  }else{#bootstrap CIs
    pred <- stats::predict(model,
                           newdata=new_data,
                           type='response')
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
    ylab = 'Expected Response'
  }
  form <- stats::as.formula(paste("preds ~ ", predictor))
  form2 <- stats::as.formula(paste("CIl + CIu ~ ", predictor))

  #make plot if categorical predictor
  if (class(new_data[,predictor]) %in% c('factor', 'character')){
  if (conf_int){
  P <- ggformula::gf_point(form, data=new_data, ...) %>%
    ggformula::gf_labs(x=xlab, y=ylab) %>%
    ggformula::gf_errorbar(form2, data=new_data, ...)
  }else{
  P <- ggformula::gf_point(form, data=new_data, ...) %>%
    ggformula::gf_labs(x=xlab, y=ylab)
  }
  }else{#make plot if quant predictor
  if (conf_int){
  P <- ggformula::gf_line(form, data=new_data, ...) %>%
    ggformula::gf_labs(x=xlab, y = ylab) %>%
    ggformula::gf_ribbon(form2, data=new_data)
  }else{
   P <- ggformula::gf_line(form, data=new_data, ...) %>%
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

