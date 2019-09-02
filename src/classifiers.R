#' @description funtion to set a new k value for each dataset, the value is
#'  the sqrt(number of samples)
#'
#' @param database the current databese.
#'
attKValue <- function(database) {
  param <- list(control = Weka_control(K = as.integer(sqrt(nrow(database))),
                                       X = T))
  obj[4] <<- c(learner("IBk", param))
}

#' @description Predicted values of all instances of the data
#'
#' @param m model
#' @param d data
#' @param funcType the type of the function for each classifier
#'
generatePredict <- function(m, d, funcType) {
  p <- predict(m, d, type = funcType)
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p, 1, which.max)]
  col2 <- apply(p, 1, max)
  data.frame(cl = col1, p = col2, id = row.names(d))
}
