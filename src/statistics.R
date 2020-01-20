#' @description Calculate the acc of the main diagonal of the matrix.
#'
#' @param cm The confusion matrix of a model.
#'
#' @return The accuracy.
#'
getAcc <- function(cm) {
  acc <- ((sum(diag(cm)) / sum(cm)) * 100)
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
  for (i in 1:ncol(cm)) {
    tryCatch({
        tp = cm[i, i]
      }, error = function(e) {
        tp <- 0
      }
    )
    fp <- sum(cm[i,]) - tp
    if (!((fp == 0) && (tp == 0))) {
      preci <- c(preci, (tp / (tp + fp)))
    } else {
      preci <- c(preci, 0)
    }
  }
  return(mean(preci))
}

#' @description Calculate the recall using the confusion matrix.
#'
#' @param cm The confusion matrix of a model.
#'
#' @return The recall.
#'
recall <- function(cm) {
  recal <- c()
  for (i in 1:ncol(cm)) {
    tryCatch(
      {
        tp = cm[i, i]
      }, error = function(e){
        tp <- 0
      }
    )
    fn <- sum(cm[,i]) - tp
    if (!((fn == 0) && (tp == 0))) {
      recal <- c(recal, (tp / (tp + fn)))
    } else {
      recal <- c(recal, 0)
    }
  }
  return(mean(recal))
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