fixCM <- function(cm) {
  if (nrow(cm) < ncol(cm)) {
    truePos <- match(rownames(cm), colnames(cm))
    newCM <- matrix(rep(0,2*ncol(cm)), nrow = ncol(cm), ncol = ncol(cm))
    colnames(newCM) <- colnames(cm)
    rownames(newCM) <- colnames(cm)
    newCM[truePos,] <- cm
  } else {
    truePos <- match(colnames(cm), rownames(cm))
    newCM <- matrix(rep(0,2*nrow(cm)), nrow = nrow(cm), ncol = nrow(cm))
    colnames(newCM) <- rownames(cm)
    rownames(newCM) <- rownames(cm)
    newCM[, truePos] <- cm
  }
  return(newCM)
}

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
  for (i in 1:ncol(cm)) {
    tryCatch({
      tp = cm[i, i]
    }, error = function(e) {
      tp = 0
    })
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
    tryCatch({
        tp = cm[i, i]
      }, error = function(e){
        tp = 0
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