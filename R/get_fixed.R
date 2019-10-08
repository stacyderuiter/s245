#' make prediction plots for lm and glm objects using ggformula
#'
#' @param data a data frame, or a fitted model object (from which the data will be extracted)
#'
#' @export
#' @return a data frame with one row, containing the same variables as data, in which each variable takes on its median value (continuous variables) or its most commonly observed value (categorical variables)
#'
#'
#'

get_fixed <- function(data){
  if (all('data.frame' %in% class(data)) == FALSE){
    if ("glmmTMB" %in% class(data)){
      data <- data$frame
    }else{
      if ("glmerMod" %in% class(data) | "lmerMod" %in% class(data)){
        data <- data@frame
      }else{
        data <- data$model
      }
    }
    namez <- names(data)
    # deal with offsets
    if (sum(stringr::str_detect(names(data), stringr::fixed("offset(log(")))){
    data[,stringr::str_detect(names(data), stringr::fixed("offset(log("))] <-
      exp(data[,stringr::str_detect(names(data), stringr::fixed("offset(log("))] )
    }
    namez <- namez %>%
      stringr::str_remove(stringr::fixed("offset(log(")) %>%
      stringr::str_remove(stringr::fixed("))"))
#    data <- data.frame(data[,2:ncol(data)]) #don't include response
    names(data) <- namez
  }



  data_out <- data.frame(data[1,]) #set up shape
  data_out[,] <- NA
  ci <- unlist(lapply(data,class)) %in% c('character', 'factor')
  qi <- which(ci==FALSE)
  ci <- which(ci==TRUE)
  # find most common level of categorical
  for (v in ci){
    fv <- factor(data[,v])
    data_out[1,v] <- levels(fv)[which.max(table(fv))]
    data_out[,v] <- factor(data_out[,v], levels=levels(fv))
  }
  # find median of quant
  for (v in qi){
    data_out[1,v] <- stats::median(data[,v], na.rm=TRUE)
  }
return(data_out)
}

