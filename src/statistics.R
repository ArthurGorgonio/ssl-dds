#' @description Fix the confusion matrix to be quadratic in number of distinct
#'   classes
#'
#' @param cm The confusion matrix of a model.
#' @param all_classes A vector with all distinct classes in the data set.
#'
#' @return A new confusion matrix with fix width and height.
#'
fixCM <- function(cm, all_classes) {
  true_cm <- matrix(rep(0, length(all_classes)^2), nrow=length(all_classes))
  colnames(true_cm) <- all_classes
  rownames(true_cm) <- all_classes
  true_row <- match(rownames(cm), all_classes)
  true_col <- match(colnames(cm), all_classes)
  for (i in 1:nrow(cm)) {
    for (j in 1:ncol(cm)) {
      true_cm[true_row[i], true_col[j]] <- cm[i, j]
    }
  }
  return(true_cm)
}


#' @description Calculate the accuracy of the main diagonal of the matrix.
#'
#' @param cm The confusion matrix of a model.
#'
#' @return The accuracy.
#'
getAcc <- function(cm) {
  acc <- ((sum(diag(cm)) / sum(cm)))
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
    })
    fp <- sum(cm[i,]) - tp
    if (!((fp == 0) && (tp == 0))) {
      preci <- c(preci, (tp / (tp + fp)))
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
      }
    )
    fn <- sum(cm[, i]) - tp
    if (!((fn == 0) && (tp == 0))) {
      recal <- c(recal, (tp / (tp + fn)))
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


#' @description Calculate the kappa statistics using the confusion matrix.
#'
#' @param cm The confusion matrix of a model.
#'
#' @return The kappa statistics..
#'
kappa <- function(cm) {
  acc <- getAcc(cm)
  prob <- c()
  for(i in 1:nrow(cm)) {
    prob <- c(prob, sum(cm[, i] / sum(cm)))
    prob <- c(prob, sum(cm[i,] / sum(cm)))
  }
  mult_prob <- prob[c(T, F)] * prob[c(F, T)]
  return((acc - sum(mult_prob)) / (1 - sum(mult_prob)))
}
