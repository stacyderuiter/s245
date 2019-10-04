#' QAIC (easier) for quasi-poisson models
#'
#' @param model a fitted model object (or optionally more than one) with family=quasipoisson
#' @param \dots additional arguments to pass to MuMIn::QAIC()
#' @export
#' @return The QAIC for the model(s). See help for dredge() in package MuMIn for details.
#'
#'
#'

myQAIC <- function(model, ...){
  # thanks to Ben Bolker:
  # https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
  if (!missing(...)){#if multiple models
    dots.xq <- lapply(list(model, ...), function(x) stats::update(x, family='x.quasipoisson'))
    dots.noq <- lapply(list(model, ...), function(x) stats::update(x, family='poisson'))
    chats <- lapply(dots.noq, s245::dfun)
    result <- mapply(MuMIn::QAIC,
                     object=dots.xq,
                     chat = chats)
    # c-hat parameter passed to the QAIC function from package MuMIn This is the variance inflation factor, here estimated using the dispersion parameter from the quasi-poisson model.
    Call <- match.call()
    Call$k <- NULL
    names(result) <- as.character(Call[-1L])
    result <- data.frame(result)
      }else{#if only one model
    xq.model <- stats::update(model, family='x.quasipoisson')
    noq.model <- stats::update(model, family='poisson')
    result <- MuMIn::QAIC(xq.model, chat = dfun(noq.model))
  }
  return(result)
}
