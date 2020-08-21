#' plot ACF for model residuals (or any vector of observations) using ggformula.
#'
#' @param formula a formula of the form ~ x, where x is usually a fitted regression model object. Supported types include those created by lm(), or glm(), geeglm(), glmmTMB(), model.avg(), gam(), and others that have a resid() method. If there is not, x will be treated as a vector of observations for which to compute the ACF.
#'
#' @export
#' @importFrom magrittr %>%
#' @return A ggplot2 plot. For additional customization, pipe (%>%) to, for example, gf_refine(), gf_labs(), gf_lims, gf_theme().


gf_acf <- function(formula){
  if (length(all.vars(formula)) != 1){
    stop("gf_acf requires a one-sided formula like: ~my_model. (Don't forget the ~)\n")
  }
  r <- try(rlang::f_rhs(formula) %>%
             eval() %>%
             stats::resid(),
           silent = TRUE)
  if (class(r) == 'try-error'){
    r <- rlang::f_rhs(formula)
  }

  acf_out <- stats::acf(r, plot = FALSE)
  acf_data <- data.frame(lag = as.numeric(acf_out$lag),
                         acf = as.numeric(acf_out$acf))
  cilim <- stats::qnorm((1 - 0.95)/2) / sqrt(length(r))
  ggformula::gf_segment(0 + acf ~ lag + lag, data = acf_data) %>%
    ggformula::gf_hline(yintercept = ~cilim,
                        linetype = 'dashed') %>%
    ggformula::gf_hline(yintercept = ~(-cilim),
                        linetype = 'dashed') %>%
    ggformula::gf_labs(x = 'Lag', y = 'Residual ACF')
}

