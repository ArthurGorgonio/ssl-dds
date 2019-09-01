#' @description Function to define constants in all code
#'
defines <- function() {
  classe <<- "class"
  funcType <<- c("raw", "prob", "probability", "probability")
  extention <<- ".csv"
  obj <<- c(learner("naiveBayes", list()), learner("JRip", list()),
            learner("rpartXse", list(se = 0.5)),
            learner("IBk", list(control = Weka_control(K = 3, X = TRUE))))
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