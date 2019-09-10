# Calculate the acc value of the training samples
calcLocalAcc <- function(c, initialLabeledDB) {
  if (c == 1) {
    classifier <- naiveBayes(as.factor(class) ~ ., trainSet)
  } else if (c == 2) {
    classifier <- rpartXse(as.factor(class) ~ ., trainSet)
  } else if (c == 3) {
    classifier <- JRip(as.factor(class) ~ ., trainSet)
  } else if (c == 4) {
    classifier <- IBk(as.factor(class) ~ ., trainSet)
  }
  matriz <- table(predict(classifier, initialLabeledDB), initialLabeledDB$class)
  localAcc <- ((sum(diag(matriz)) / length(initialLabeledDB$class)) * 100)
  return(localAcc)
}

# Return the confusion matrix
confusionMatrix <- function(model, testDB) {
  colunsNames <- colnames(testDB)
  dbClassOff <- match("class", colunsNames)
  testData <- testDB[, -dbClassOff]
  type <- "class"
  testDBClass <- testDB$class
  confusion <- table(predict(model, testData, type), testDBClass)
  return(confusion)
}

# Convert each sample in probPreds in character
convertProbPreds <- function(probPreds) {
  aux <- sapply(probPreds, is.factor)
  probPreds[aux] <- lapply(probPreds[aux], as.character)
  return(probPreds)
}

generateModel <- function(learner, form, data, sup) {
  model <- runLearner(learner, form, data[sup, ])
  return(model)
}

# Generate a matrix with the sample, class and confidence value
generateProbPreds <- function(predFunc, model, data, sup) {
  probPreds <- generatePredict(model, data[-sup, ], predFunc)
  return(probPreds)
}

# Calculate the acc and return
getAcc <- function(matrix, all) {
  acc <- ((sum(diag(matrix)) / all) * 100)
  return(acc)
}

getDatabase <- function(datasetName) {
  database <- read.arff(paste("../bases", datasetName, sep = "/"))
  return(database)
}

getRealId <- function(dataset, sup) {
  ids <- as.integer(rownames(dataset))
  return(ids[-sup])
}

#' @description Function to define constants in all code
#'
defines <- function() {
  class <<- "class"
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
#' @param labeledDB the full dataset without changes
#' @param trainId the vector with the selected samples
#'
#' @return a new dataset with some percents of the samples have the NA in class
#' atribute
#'
newBase <- function(labeledDB, trainId){
  labeledDB[-trainId, "class"] <- NA
  return(labeledDB)
}

# Return the new confidence value changed by the cr value
#cr=5 nem umas das condiçoes vao ser aceitas
newConfidence <- function(localAcc, threshold, confValue) {
  crRatio <- cr / 100
  if ((localAcc > (threshold + 1)) && ((confValue - crRatio) > 0.0)) {
    confValue <- confValue - crRatio
  } else if ((localAcc < (threshold - 1)) && ((confValue + crRatio) <= 1)) {
    confValue <- confValue + crRatio
  }
  return(confValue)
}

# Search in the 'moda' vector the higger value of the sample (sum or vote)
searchClass <- function(i, moda) {
  return(colnames(moda)[which.max(moda[i, ])])
}

# Storage the vote of the classifier each iteration
storageFashion <- function(probPreds, moda) {
  distClass <- unique(originalDB$class)
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

# Storage the sum of the confidence for each iteration
storageSum <- function(probPreds, moda) {
  distClass <- unique(originalDB$class)
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
#' @param cl the choosen classifier
#' @param initialLabeledDB the dataset with the initial samples labeled.
#'
#' @return Return the accuracy of the dataset with the initial samples
#' labeled.
#'
supAcc <- function(cl, initialLabeledDB){
  std <- supModel(cl, initialLabeledDB)
  supConfusionMatrix <- confusionMatrix(std, initialLabeledDB)
  return(getAcc(supConfusionMatrix, sum(supConfusionMatrix)))
}

#' @description A supervised model trained with the initial samples.
#'
#' @param cl the classifier to be used.
#' @param initialLabeledDB the dataset with the initial samples labeled.
#'
#' @return Return a supervised classifier.
#'
supModel <- function(cl, initialLabeledDB){
  form = as.formula(paste(class, '~', '.'))
  switch(as.character(cl),
          '1' = std <- naiveBayes(form, initialLabeledDB),
          '2' = std <- JRip(form, initialLabeledDB),
          '3' = std <- rpartXse(form, initialLabeledDB, se = 0.5),
          '4' = std <- IBk(form, initialLabeledDB,
                         control = Weka_control(K = as.integer(sqrt(
                                          nrow(initialLabeledDB))), X = TRUE))
  )
  return(std)
}

#' @description Check if the classification if valid.
#' se o treino for válido, a função apenas atribui o conj de treinamento antigo
#' ao novo conj. de treinamento e limpa o conj. antigo.
#' se o treino não for válido, a função junta o conj de treinamento antigo com o
#' novo e chama a funcao validTraining para validar se os dois conjuntos juntos
#' podem ser treinados.
#'
#' @param validTrainIt boolean for check if it's a valid train.
#' @param in_conj_treino vector with the samples to train.
#' @param oldTrainSetIds old vector whit the samples to train.
#' @param data the dataset with all samples.
#' @param nClass the total of the classes in the dataset.
#' @param minSamplesClass the min samples of each class for training.
#'
#' @return a boolean to say if the classification is valid.
#'
#' TODO review this comment
#'
validClassification <- function(validTrainIt, localOldTrainSet,
                                localTrainSet, nClass, minSamplesClass) {
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
#' @param data the all dataset.
#' @param trainSetIds vector with the samples selectedes to train.
#' @param Nclass the total of the classes in the dataset.
#' @param minSamplesClass the min samples of each class for training.
#'
#' @return a boolean to say if the training is valid.
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

whichDB <- function(pattern) {
  file <- list.files(pattern = pattern)
  if (length(file) != 0) {
    bd <- readFile(file)
    return(as.integer((nrow(bd) / 10) + 1))
  }
}
