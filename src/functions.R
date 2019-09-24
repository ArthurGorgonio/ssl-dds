#' @description Calculate the acc value of the training samples.
#'
#' @param c Classifier to be used.
#' @param iniLabDB The data set with samples select before FlexConC call.
#' @param trainSet The samples are select in a moment.
#'
#' @return Accuracy of the model trained with a sample set `trainSet`.
#'
calcLocalAcc <- function(c, iniLabDB, trainSet) {
  form = as.formula(paste(label, '~', '.'))
  switch(as.character(c),
         'naiveBayes' = std <- naiveBayes(form, trainSet),
         'JRip' = std <- JRip(form, trainSet),
         'rpartXse' = std <- rpartXse(form, trainSet, se = 0.5),
         'IBk' = {
           k <- floor(sqrt(nrow(iniLabDB)))
           std <- IBk(form, trainSet, control = Weka_control(K = k, X = TRUE))
         }
  )
  matriz <- table(predict(classifier, iniLabDB), iniLabDB$class)
  localAcc <- ((sum(diag(matriz)) / length(iniLabDB$class)) * 100)
  return(localAcc)
}

#' @description Generate the confusion matrix of the model.
#'
#' @param model A trained classifier will be tested.
#' @param testDB The data set which the classifier will be evaluated.
#'
#' @return The confusion matrix.
#'
confusionMatrix <- function(model, testDB) {
  colunsNames <- colnames(testDB)
  dbClassOff <- match(label, colunsNames)
  testData <- testDB[, -dbClassOff]
  type <- label
  testDBClass <- testDB$class
  confusion <- table(predict(model, testData, type), testDBClass)
  return(confusion)
}

#' @description Convert each sample in probPreds in character
#'
#' @param probPreds A data frame which contains class | confidence ratio | id.
#'
#' @return Converted probPreds.
#'
convertProbPreds <- function(probPreds) {
  aux <- sapply(probPreds, is.factor)
  probPreds[aux] <- lapply(probPreds[aux], as.character)
  return(probPreds)
}

#' @description Create a classifier from a data set.
#'
#' @param learner A classifier will be trained.
#' @param form The formula of the features and target.
#' @param dataLab Data set which all labeled samples.
#'
#' @return A trained model.
#'
generateModel <- function(learner, form, dataLab) {
  model <- runLearner(learner, form, dataLab)
  return(model)
}

#' @description Generate a matrix with all samples than contains class,
#'  confidence rate and id of each samples in the data.
#'
#' @param model A classifier will be trained.
#' @param dataUnl Data set which all labeled samples.
#' @param predFunc The formula to the classifier use the confidence value.
#'
#' @return Generate a matrix with all samples x class | confidence value | id.
#'
generateProbPreds <- function(model, dataUnl, predFunc) {
  probPreds <- generatePredict(model, dataUnl, predFunc)
  return(probPreds)
}

#' @description Calculate the acc of the main diagonal of the matrix.
#'
#' @param matrix The confusion matrix of a model.
#' @param all The sum of the confussion matrix.
#'
#' @return The accuracy.
#'
getAcc <- function(matrix, all) {
  acc <- ((sum(diag(matrix)) / all) * 100)
  return(acc)
}

#' @description Calculate the acc of the main diagonal of the matrix.
#'
#' @param dataset The data set.
#' @param sup The ids of the labeled samples.
#'
#' @return The real ids of the data set.
#'
getRealId <- function(dataset, sup) {
  ids <- as.integer(rownames(dataset))
  return(ids[-sup])
}

#' @description Function to define constants in all code
#'
defines <- function() {
  label <<- "class"
  funcType <<- c("raw", "probability", "prob", "probability")
  extention <<- ".csv"
  obj <<- c(learner("naiveBayes", list()), learner("JRip", list()),
            learner("rpartXse", list(se = 0.5)),
            learner("IBk", list(control = Weka_control(K = 3, X = TRUE))))
  trainSet <<- c()
  training <<- c()
  accC1S <<- c()
  accC1V <<- c()
  accC2 <<- c()
  # FlexCon-C1 variables
  globalIt <<- c()
  db <<- c()
  confValue <<- c()
  globalSamplasAdd <<- c()
  percentageLabelSamples <<- c()
  globalAcc <<- c()
  glocalCorrect <<- c()
  # # FlexCon-C2 variables
  # it_g_3 <<- c()
  # bd_g_3 <<- c()
  # thrConf_g_3 <<- c()
  # nr_added_exs_g_3 <<- c()
  # tx_g_3 <<- c()
  # acc_g_3 <<- c()
  # acertou_g_3 <<- c()
  # grad_g <<- c()
  # bd <<- c()
  # tx <<- c()
}

#' @description This function set the class atribute to NA without change the
#' class of selected samples
#'
#' @usage newBase(labeledDB, trainId)
#'
#' @param labeledDB The full dataset without changes
#' @param trainId The vector with the selected samples
#'
#' @return A new dataset with some percents of the samples have the NA in class
#' atribute
#'
newBase <- function(labeledDB, trainId){
  labeledDB[-trainId, label] <- NA
  return(labeledDB)
}

#' @description Change the confidence rate using changeRate param to change the
#'  confidence and flexibilize the algorithm.
#'
#' @param localAcc Accuracy of the model trained with a sample set.
#' @param initialAcc The accuracy of the initial labeled samples.
#' @param confValue The Confidence value of the present iteration.
#' @param changeRate The factor of the change.
#'
#' @return The new confidence value changed by the `changeRate` value.
#'
newConfidence <- function(localAcc, initialAcc, confValue, changeRate) {
  crRatio <- changeRate / 100
  if ((localAcc > (initialAcc + 1)) && ((confValue - crRatio) > 0.0)) {
    confValue <- confValue - crRatio
  } else if ((localAcc < (initialAcc - 1)) && ((confValue + crRatio) <= 1)) {
    confValue <- confValue + crRatio
  }
  return(confValue)
}

#' @description Search in the 'moda' matrix, the higger value of the sample
#'  (sum or vote).
#'
#' @param i The index will be searched.
#' @param moda The matrix with the values.
#'
#' @return The label of the index `i`.
#'
searchClass <- function(i, moda) {
  return(colnames(moda)[which.max(moda[i, ])])
}

#' @description Storage the vote of the classifier in each iteration.
#'
#' @param probPreds A data frame with remaining samples.
#' @param moda The history of the choices since the first iteration.
#'
#' @return The updated `moda` matrix using fashion.
#'
storageFashion <- function(probPreds, moda) {
  distClass <- colnames(moda)
  for (x in 1:nrow(probPreds)) {
    id <- as.numeric(probPreds[x, ncol(probPreds)])
    for (y in 1:(length(distClass))) {
      if (as.character(probPreds[x, 1]) == as.character(distClass[y])) {
        moda[id, distClass[y]] <- moda[id, distClass[y]] + 1
        break
      }
    }
  }
  return(moda)
}

#' @description Storage the sum of the confidences in each iteration.
#'
#' @param probPreds A data frame with remaining samples.
#' @param moda The history of the choices since the first iteration.
#'
#' @return The updated `moda` matrix using sum of the confidences.
#'
storageSum <- function(probPreds, moda) {
  distClass <- colnames(moda)
  for (x in 1:nrow(probPreds)) {
    id <- as.numeric(probPreds[x, ncol(probPreds)])
    for (y in 1:length(distClass)) {
      if (as.character(probPreds[x, 1]) == as.character(distClass[y])) {
        moda[id, distClass[y]] <- moda[id, distClass[y]] + probPreds[x, 2]
        break
      }
    }
  }
  return(moda)
}


#' @description Make a supervised model and get the accuracy of this.
#'
#' @param cl The choosen classifier
#' @param iniLabDB The dataset with the initial samples labeled.
#'
#' @return Return the accuracy of the dataset with the initial samples
#' labeled.
#'
supAcc <- function(cl, iniLabDB) {
  std <- supModel(cl@func, iniLabDB)
  supConfusionMatrix <- confusionMatrix(std, iniLabDB)
  return(getAcc(supConfusionMatrix, sum(supConfusionMatrix)))
}

#' @description A supervised model trained with the initial samples.
#'
#' @param cl The classifier to be used.
#' @param iniLabDB The dataset with the initial samples labeled.
#'
#' @return Return a supervised classifier.
#'
supModel <- function(cl, iniLabDB) {
  form = as.formula(paste(label, '~', '.'))
  switch(as.character(cl),
         'naiveBayes' = std <- naiveBayes(form, iniLabDB),
         'JRip' = std <- JRip(form, iniLabDB),
         'rpartXse' = std <- rpartXse(form, iniLabDB, se = 0.5),
         'IBk' = {
           k <- floor(sqrt(nrow(iniLabDB)))
           std <- IBk(form, iniLabDB, control = Weka_control(K = k, X = TRUE))
         }
  )
  return(std)
}

#' @description Check if the classification is valid. If the train is not valid, 
#'  combine all sets and try to train again.
#'
#' @param validTrainIt Check if the train is valid.
#' @param localOldTrainSet Old vector with the samples to train.
#' @param localTrainSet A vector with the samples to train.
#' @param nClass The total of the classes in the dataset.
#' @param minSamplesClass The min samples of each class for training.
#'
#' @return Logical return if the classification is valid.
#'
validClassification <- function(validTrainIt, localOldTrainSet, localTrainSet,
                                nClass, minSamplesClass) {
  if (validTrainIt) {
    trainSet <<- localTrainSet
    OldTrainSet <<- c()
    changeTrainSet <<- TRUE
    return(TRUE)
  } else if (!is.null(nrow(localOldTrainSet))) {
    trainSet <<- rbind(localTrainSet, localOldTrainSet)
    validTrainIt <- validTraining(trainSet, nClass, minSamplesClass)
    changeTrainSet <<- TRUE
    if (validTrainIt) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @description Check if exists a min accetable samples per class.
#' o treino só é válido se todas as classes estiverem representadas no conj.
#' de treimento e se a quantidade de exemplos de cada classe for no mínimo (a qtdade
#' de exemplos da classe com menor representação no conj. ini. rot.?)
#'
#' @param data The data set with all samples.
#' @param trainSetIds The ids of the selected samples to be used in train.
#' @param Nclass The number of the distinct classes in the data set.
#' @param minSamplesClass The min samples of each class that training require.
#'
#' @return Logical return training is valid.
#'
validTraining <- function(data, trainSetIds, Nclass, minSamplesClass) {
  samplesClass <- ddply(data[trainSetIds, ], ~class, summarise,
                        distictClass = length(class))
  if (NROW(samplesClass) == Nclass) {
    for (x in 1:NROW(samplesClass)) {
      if (samplesClass$distictClass[x] < minSamplesClass) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  return(FALSE)
}
