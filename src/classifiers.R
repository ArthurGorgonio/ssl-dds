#' @description funtion to set a new k value for each dataset, the value is
#'  the sqrt(number of samples)
#'
#' @param dataset the current databese.
#'
attKValue <- function(dataset) {
  param <- list(control = Weka_control(K = floor(sqrt(nrow(dataset))), X = T))
  obj[4] <<- c(learner("IBk", param))
}

#' @description Predicted values of all instances of the data
#'
#' @param model the current model
#' @param data the current dataset
#' @param funcType the type of the function for each classifier
#'
generatePredict <- function(model, data, funcType) {
  pred <- predict(model, data, type = funcType)
  col1 <- colnames(pred)[apply(pred, 1, which.max)]
  col2 <- apply(pred, 1, max)
  return(data.frame(cl = col1, pred = col2, id = row.names(data)))
}
