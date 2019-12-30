#' @description Calculate the acc of the main diagonal of the matrix.
#'
#' @param matrix The confusion matrix of a model.
#'
#' @return The accuracy.
#'
getAcc <- function(matrix) {
  acc <- ((sum(diag(matrix)) / sum(matrix)) * 100)
  return(acc)
}


#' @description Calculate the precision using the confusion matrix.
#'
#' @param cm The confusion matrix of a model.
#'
#' @return The precision.
#'
precision <- function(cm) {
  preci <- c()
  for (i in 1:nrow(cm)) {
    tp <- cm[i, i]
    fp <- sum(cm[i,]) - tp
    if (!(fp == 0) && (tp == 0)) {
      preci <- c(preci, (tp / (tp + fp)))
    } else {
      preci <- c(preci, 0)
    }
  }
  return(sum(preci) / nrow(cm))
}

#' @description Calculate the recall using the confusion matrix.
#'
#' @param cm The confusion matrix of a model.
#'
#' @return The recall.
#'
recall <- function(cm) {
  recal <- c()
  for (i in 1:nrow(cm)) {
    tp <- cm[i, i]
    fn <- sum(cm[,i]) - tp
    recal <- c(recal, (tp / (tp + fn)))
  }
  return(sum(recal) / nrow(cm))
}

#' @description Calculate the f-measure using the confusion matrix.
#'
#' @param cm The confusion matrix of a model.
#'
#' @return The f-measure.
#'
fmeasure <- function(cm) {
  pre <- precision(cm)
  recal <- recall(cm)
  return(2 * ((pre * recal) / (pre + recal)))
}