#' @description Select the best model to compose the ensemble of classifiers.
#'
#' @param ensemble The ensemble.
#' @param trainedModels A list of trained models that will be used to choose the
#'  best one.
#' @param accuracy A vector with the accuracy per model in `trainedModels`.
#'
#' @return A new `ensemble` aggregating the best model aat ensemble. 
#'
addingEnsemble <- function(ensemble, trainedModels, accuracy) {
  list <- c(ensemble, trainedModels[which.max(accuracy)])
  return(list)
}

#' @description Funtion to set a new k value for each dataset, the value is
#'  the sqrt(number of samples).
#'
#' @param dataset The current databese.
#'
#' @return A new classifier IBk using the right k.
#'
attKValue <- function(dataset) {
  param <- list(control = Weka_control(K = floor(sqrt(nrow(dataset))), X = T))
  return(c(learner("IBk", param)))
}

#' @description Drop all trained models of the ensemble.
#'
#' @param ensemble The ensemble.
#'
#' @return Removing all classifiers of the ensemble.
#'
dropEnsemble <- function(ensemble) {
  return(list())
}

#' @description Predicted values of all instances of the data.
#'
#' @param model The current model.
#' @param data The current dataset.
#' @param funcType The type of the function for each classifier.
#'
#' @return A dataframe with 3 columns the class, confidence value of the class
#'  and the id of the sample in data.
#'
generatePredict <- function(model, data, funcType) {
  pred <- predict(model, data, type = funcType)
  col1 <- colnames(pred)[apply(pred, 1, which.max)]
  col2 <- apply(pred, 1, max)
  return(data.frame(cl = col1, pred = col2, id = row.names(data)))
}

#' @description Measure the accuracy per classifier in ensemple on labeled
#'  data.
#'
#' @param ensemble The ensemble of classifiers which be used.
#' @param dataL The current labeled batch of the data stream.
#'
#' @return A vector with the accuracy per classifier.
#'
measureEnsemble <- function(ensemble, dataL) {
  accPerClassifier <- c()
  for (classi in ensemble) {
    cm <- confusionMatrix(classi, dataL)
    acc <- getAcc(cm)
    accPerClassifier <- c(accPerClassifier, acc)
  }
  return(accPerClassifier)
}

#' @description Select the worst model of the ensemble and remove it.
#'
#' @param ensemble The ensemble.
#' @param dataL The current labeled batch of the data stream.
#'
#' @return A new `ensemble` with the worst model removed of the ensemble. 
#'
removingEnsemble <- function(ensemble, dataL) {
  classifiers <- measureEnsemble(ensemble, dataL)
  return(ensemble[-which.min(classifiers)])
}

#' @description Select the worst model of the ensemble and swap with best oracle.
#'
#' @param ensemble The ensemble.
#' @param dataL The current labeled batch of the data stream.
#' @param trainedModels A list of trained models that will be used to choose the
#'  best one.
#' @param accuracy A vector with the accuracy per model in `trainedModels`.
#'
#' @return A new `ensemble` with the worst model removed and adding the best
#'  oracle in the ensemble.
#'
swapEnsemble <- function(ensemble, dataL, trainedModels, accuracy) {
  ensemble <- removingEnsemble(ensemble, dataL)
  ensemble <- addingEnsemble(ensemble, trainedModels, accuracy)
  return(ensemble)
}
