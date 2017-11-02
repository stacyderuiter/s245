#' make prediction plots for lm and glm objects using ggformula.
#'
#' @param model a fitted model object created by lm() or glm()
#' @param predictor the covariate for which to make predictions. other predictors in the model will be held constant at their median value, or the most commonly observed value in the dataset.
#' @param xlab X axis label for plot (defaults to name of predictor variable)
#' @param ylab Y axis label for plot (defaults to "Predictions from Fitted Model")
#' @param conf_int logical: should 95 percent confidence intervals be shown (as error bars or confidence band)?
#' @param new_data_out logical: should data set used for predictions be output? If TRUE, result is a list including the plot object and the data frame.
#' @param ... Additional arguments to be passed to plotting function
#'
#' @export
#' @return A ggplot2 plot created using ggformula gf_line or gf_point


pred_plot <- function(model, predictor, xlab=NULL, ylab=NULL,
                      conf_int=TRUE, ...){
  fixed_vals <- get_fixed(model)
  data <- model$model
  #make dataset for predictions
  new_data <- data.frame(x=seq(from=min(data[,predictor], na.rm=TRUE),
                               to = max(data[,predictor], na.rm=TRUE),
                               length.out=250))
  xi <- which(names(fixed_vals)==predictor)
  new_data[,c(2:(ncol(data)-1))] <- fixed_vals[,-xi]
  #make predictions
  pred <- predict(model, newdata=new_data, type='response', se.fit=TRUE)
  new_data$preds <- pred$fit
  if (conf_int){
  new_data$CIl <- pred$fit - 1.96*pred$se.fit
  new_data$CIu <- pred$fit + 1.96*pred$se.fit
  }

  if (is.null(xlab)){
    xlab = predictor
  }
  if (is.null(ylab)){
    ylab = 'Predictions from Fitted Model'
  }
  #make plot if categorical predictor
  if (conf_int){
  gf_point(preds ~ x, data=new_data) %>%
    gf_labs(x=xlab, y=ylab) %>%
    gf_errorbar(CIl + CIu ~ x, data=new_data)
  }else{
    gf_point(preds ~ x, data=new_data) %>%
      gf_labs(x=xlab, y=ylab)
  }
  #make plot if quant predictor
  if (conf_int){
  gf_line(preds ~ x, data=new_data) %>%
    gf_labs(x=xlab, y = ylab) %>%
    gf_ribbon(CIl + CIu ~ x, data=new_data)
  }else{
    gf_line(preds ~ x, data=new_data) %>%
      gf_labs(x=xlab, y = ylab)
  }

}

