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
    #if data is a fitted model object, extract data
    data <- data$model
  }
  data_out <- data[1,] #set up shape
  data_out[,] <- NA
  ci <- unlist(lapply(data,class)) %in% c('character', 'factor')
  qi <- which(ci==FALSE)
  ci <- which(ci==TRUE)
  # find most common level of categorical
  for (v in ci){
    fv <- factor(data[,v])
    data_out[1,v] <- levels(fv)[which.max(table(fv))]
  }
  # find median of quant
  for (v in qi){
    data_out[1,v] <- median(data[,v], na.rm=TRUE)
  }
return(data_out)
}

