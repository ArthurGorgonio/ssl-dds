#' ==============================================================
#' CLASS: learner
#' ==============================================================
#' Luis Torgo, Jan 2009
#' ==============================================================
#' 
setClass("learner", representation(func="character",pars="list"))


#' --------------------------------------------------------------
#' constructor function
learner <- function(func,pars=list()) {
  if (missing(func)) stop("\nYou need to provide a function name.\n")
  new("learner",func=func,pars=pars)
}

# show
setMethod("show","learner", function(object) {
  cat('\nLearner:: ',deparse(object@func),'\n\nParameter values\n')
  for(n in names(object@pars))
    cat('\t',n,' = ',deparse(object@pars[[n]]),'\n')
  cat('\n\n')
  }
)

#' =====================================================
#' Function that can be used to call a learning system
#' whose information is stored in an object of class learner.
#' =====================================================
#' Luis Torgo, Fev 2009
#' =====================================================
#' Example run:
#' l  <- learner('nnet',pars=list(size=4,linout=T))
#' runLearner(l,medv ~ ., Boston)
#'
runLearner <- function(l,...) {
  if (!inherits(l,'learner')) stop(l,' is not of class "learner".')
  do.call(l@func,c(list(...),l@pars))
}



#' @description Select the best model to compose the ensemble of classifiers.
#'
#' @param ensemble The ensemble.
#' @param newClassifier A new trained classifier to compose the ensemble.
#'
#' @return A new `ensemble` aggregating the best model at ensemble. 
#'
addingEnsemble <- function(ensemble, newClassifier) {
  ensemble[[length(ensemble) + 1]] <- newClassifier
  return(ensemble)
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

#' @description Measure the accuracy per classifier in ensemble on labeled
#'  data.
#'
#' @param ensemble The ensemble of classifiers which be used.
#' @param dataOracle The current labeled batch using oracle classifier.
#'
#' @return A vector with the accuracy per classifier.
#'
measureEnsemble <- function(ensemble, dataOracle) {
  accPerClassifier <- c()
  for (classi in ensemble) {
    cm <- confusionMatrix(classi, dataOracle)
    acc <- getAcc(cm)
    accPerClassifier <- c(accPerClassifier, acc)
  }
  return(accPerClassifier)
}

#' @description Generate the precition on the dataset using a model.
#'
#' @param model A trained classifier will be tested.
#' @param testDB The data set which the classifier will be evaluated.
#'
#' @return A dataset set predicted using the model.
#'
predictClass <- function(model, testDB, type = "class") {
  colunsNames <- colnames(testDB)
  dbClassOff <- match("class", colunsNames)
  testData <- testDB[, -dbClassOff]
  prediction <- predict(model, testData, type)
  return(prediction)
}

predictEnsemble <- function(ensemble, oracleDB, all_levels) {
  classPred <- generateMemory(oracleDB, length(all_levels), all_levels)
  for (cl in ensemble) {
    pred <- predictClass(cl, oracleDB)
    pos <- match(pred, colnames(classPred))
    for (sample in 1:length(pos)) {
      classPred[sample, pos[sample]] <- classPred[sample, pos[sample]] + 1
    }
  }
  allClassify <- c()
  for (sample in 1:length(pos)) {
    if (length(which(classPred[sample,] == max(classPred[sample,]))) == 1) {
      allClassify <- c(allClassify, which.max(classPred[sample,]))
    } else{
      allClassify <- c(allClassify, classPred[sample, ]
                       [sample(1:length(which(
                         classPred[sample,] == max(classPred[sample,]))), 1)])
    }
  }
  ensemblePred <- factor(names(allClassify), levels(oracleDB$class))
  return(ensemblePred)
}



predictEnsembleConfidence <- function(ensemble, ensemble_weights, oracleDB,
                                      all_levels) {
  classPred <- generateMemory(oracleDB, length(all_levels), all_levels)
  for (cl in 1:length(ensemble)) {
    pred <- predictClass(ensemble[[cl]], oracleDB, "probability") * ensemble_weights[cl]
    pos <- match(colnames(pred),colnames(classPred))
    for(j in 1:length(pos)){
      classPred[,j] <- classPred[,j] + pred[,pos[j]]
    }
  }
  allClassify <- c()
  for (sample in 1:nrow(oracleDB)) {
    if (length(which(classPred[sample,] == max(classPred[sample,]))) == 1) {
      allClassify <- c(allClassify, which.max(classPred[sample,]))
    } else{
      allClassify <- c(allClassify, classPred[sample, ]
                       [sample(1:length(which(
                         classPred[sample,] == max(classPred[sample,]))), 1)])
    }
  }
  ensemblePred <- factor(names(allClassify), levels(oracleDB$class))
  return(ensemblePred)
}

#' @description Select the worst model of the ensemble and remove it.
#'
#' @param ensemble The ensemble.
#' @param dataOracle The current labeled batch of the data stream.
#'
#' @return A new `ensemble` with the worst model removed of the ensemble.
#'
removingEnsemble <- function(ensemble, dataOracle) {
  classifiers <- measureEnsemble(ensemble, dataOracle)
  return(ensemble[-which.min(classifiers)])
}

#' @description Select the worst model of the ensemble and swap with best oracle.
#'
#' @param ensemble The ensemble.
#' @param dataOracle The current labeled batch of the data stream.
#' @param oracle the new classifier to be swapped with the worst classifier.
#'
#' @return A new `ensemble` with the worst model removed and adding the best
#'  oracle in the ensemble.
#'
swapEnsemble <- function(ensemble, dataOracle, oracle) {
  ensemble <- removingEnsemble(ensemble, dataOracle)
  ensemble <- addingEnsemble(ensemble, oracle)
  return(ensemble)
}


weightEnsemble <- function(ensemble, oracleDB, all_levels, type = "acc") {
  classfiers_weights <- c()
  for (cl in ensemble) {
    cm <- confusionMatrix(cl, oracleDB)
    switch (type,
      "acc" = value <- getAcc(cm),
      "fmeasure" = value <- fmeasure(cm)
    )
    classfiers_weights <- c(classfiers_weights, value)
  }
  return(classfiers_weights)
}
