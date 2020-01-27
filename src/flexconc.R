#' @description This function is a basic setup to other functions.
#'
#' @param data1It A data frame with all samples seted first iteration.
#' @param dataXIt A data frame with remaining samples.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param memo The history of the choices since the first iteration.
#' @param comp The insertion rule to be used in the comparation.
#'
#' @return The selected samples using a matching insertion rule.
#'
basicCheck <- function(data1It, dataXIt, confValue, memo, comp) {
  samplesData <- c()
  pos <- 0
  xId <- c()
  yCl <- c()
  zConfPred <- c()
  lvls <- match(dataXIt$id, data1It$id)
  for (index in 1:length(lvls)) {
    if (letCheck(data1It, dataXIt, confValue, lvls[index], index, comp)) {
      pos <- pos + 1
      xId[pos] <- as.numeric(levels(dataXIt$id))[dataXIt$id[index]]
      if ((comp == "1") || (comp == "2")) {
        yCl[pos] <- as.factor(dataXIt$cl[index])
      } else {
        yCl[pos] <- as.factor(searchClass(xId[pos], memo))
      }
      zConfPred[pos] <- dataXIt[index, 2]
    }
  }
  samplesData <- data.frame(cl = yCl, prob = zConfPred, id = xId)
  return(samplesData)
}

#' @description The rule is: Both samples have the same class and both 
#'  confidences are higher than threshold `confValue`.
#'
#' @param data1It A data frame with all samples seted first iteration.
#' @param dataXIt A data frame with remaining samples.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param index1It The sample from data1It that will be compared.
#' @param index The sample from dataXIt that will be compared.
#'
#' @return Logical return if rule is satisfied.
#'
classCheck <- function(data1It, dataXIt, confValue, index1It, index) {
  if ((as.character(data1It[index1It, 1]) == as.character(dataXIt[index, 1]))) {
    if ((data1It[index1It, 2] >= confValue)
        && (dataXIt[index, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @description The rule is: Both samples have the same class and just one 
#'  confidences are higher than threshold `confValue`.
#'
#' @param data1It A data frame with all samples seted first iteration.
#' @param dataXIt A data frame with remaining samples.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param index1It The sample from data1It that will be compared.
#' @param index The sample from dataXIt that will be compared.
#'
#' @return Logical return if rule is satisfied.
#'
confCheck <- function(data1It, dataXIt, confValue, index1It, index) {
  if ((as.character(data1It[index1It, 1]) == as.character(dataXIt[index, 1]))) {
    if ((data1It[index1It, 2] >= confValue)
        || (dataXIt[index, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @description The rule is: Both samples do not have the same class, but both 
#'  confidences are higher than threshold `confValue`.
#'
#' @param data1It A data frame with all samples seted first iteration.
#' @param dataXIt A data frame with remaining samples.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param index1It The sample from data1It that will be compared.
#' @param index The sample from dataXIt that will be compared.
#'
#' @return Logical return if rule is satisfied.
#'
diffClassCheck <- function(data1It, dataXIt, confValue, index1It, index) {
  if ((as.character(data1It[index1It, 1]) != as.character(dataXIt[index, 1]))) {
    if ((data1It[index1It, 2] >= confValue)
        && (dataXIt[index, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @description  The rule is: Both samples do not have the same class and both 
#'  confidences are lower than threshold `confValue`.
#'
#' @param data1It A data frame with all samples seted first iteration.
#' @param dataXIt A data frame with remaining samples.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param index1It The sample from data1It that will be compared.
#' @param index The sample from dataXIt that will be compared.
#'
#' @return Logical return if rule is satisfied.
#'
diffConfCheck <- function(data1It, dataXIt, confValue, index1It, index) {
  if ((as.character(data1It[index1It, 1]) != as.character(dataXIt[index, 1]))) {
    if ((data1It[index1It, 2] >= confValue)
        || (dataXIt[index, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @description Generate a memorization matrix to store the label using some
#'  condition (i.e. vote or sum of confidences).
#'
#' @param rawData The dataset of the unlabel and label samples.
#' @param nClass The total of the classes in the dataset.
#'
#' @return A matrix (number of samples x number of distinct classes).
#'
generateMemory <- function(rawData, nClass) {
  memo <- matrix(rep(0, nrow(rawData)), nrow(rawData), nClass, FALSE,
                 list(rownames(rawData), sort(levels(rawData$class))))
  rm(rawData)
  return(memo)
}

#' @description The Flexive Confidence with Classifier (FlexCon-C) algorithm. It
#'  is a semi-supervised algorithm based on self-training. It uses a classifier
#'  to change the threshold and flexibilize the number of samples which be
#'  classified in the iteration.
#'
#' @assume The target of the data is called `class`.
#'
#' @param learner A classifier model to be trained each iteration.
#' @param predFunc The function that classifier predict the class of a sample
#'  and the confidence rate of the prediction.
#' @param classDist The matrix with the amount of the samples per class.
#' @param initialAcc The accuracy of the initial labeled samples.
#' @param method The choosed algorithm (FlexCon-C1(s), FlexCon-C1(v) or 
#'  FlexCon-C2)
#' @param data The data set with labeled and unlabeled samples.
#' @param sup Ids of the labeled samples.
#' @param classiName The numeber of the classifier to train a sup model.
#' @param cr The changeRate param to flexibility this algorithm.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param maxIts The max number of iterations.
#'
#' @return A trained `model` to classify samples.
#'
flexConC <- function(learner, predFunc, classDist, initialAcc, method, data,
                     sup, classiName, cr = 5, confValue = 0.95, maxIts = 100) {
  defaultSup <- sup
  it <- 0
  minClass <- floor(min(classDist$samplesClass) * 0.1)
  nClass <- nrow(classDist)
  trainSetIds <- c()
  oldTrainSetIds <- c()
  # FlexCon-C1 only
  if ((method == "1") || (method == "2")) {
    memo <- generateMemory(data, nClass)
  }
  addRotSuperv <- FALSE
  while ((it < maxIts) && (length(sup) <= nrow(data))) {
    newSamples <- c()
    it <- it + 1
    model <- generateModel(learner, form, data[sup, ])
    probPreds <- generateProbPreds(model, data[-sup, ], predFunc)
    if (it > 1) {
      if (method != "3") {
        memo <- updateMemory(probPreds, memo, method)
        newSamples <- flexConC1(probPreds1It, probPreds, confValue, memo)
      } else {
        modelSup <- generateModel(learner, form, data[sup, ])
        probPredsSup <- generateProbPreds(modelSup, data[-sup, ], predFunc)
        newSamples <- flexConC2(probPreds, probPredsSup, confValue)
      }
    } else {
      probPreds1It <- probPreds
      newSamples <- probPreds[which(probPreds$pred >= confValue), ]
    }
    if ((length(newSamples) > 0) && (nrow(newSamples) > 0)) {
      trainSetIds <- match(newSamples$id, rownames(data))
      if (addRotSuperv) {
        addRotSuperv <- FALSE
      }
      data[trainSetIds, label] <- newSamples$cl
      classify <- validClassification(data, trainSetIds, oldTrainSetIds, nClass,
                                      minClass)
      sup <- c(sup, trainSetIds)
      if (classify) {
        oldTrainSetIds <- c()
        localAcc <- calcLocalAcc(classiName, data[defaultSup, ], data[sup, ])
        confValue <- newConfidence(localAcc, initialAcc, confValue, cr)
      } else {
        oldTrainSetIds <- c(oldTrainSetIds, trainSetIds)
      }
    } else {
      confValue <- max(probPreds[, 2])
    }
  }
  return(model)
}

#' @description The FlexCon-C1 method. This method uses a matrix to store all
#'  the labels of the samples.
#'
#' @param probPreds1It A data frame with all samples seted first iteration.
#' @param probPreds A data frame with remaining samples.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param memo The history of the choices since the first iteration.
#'
#' @return The ids of the selected samples by insetion rules.
#'
flexConC1 <- function(probPreds1It, probPreds, confValue, memo) {
  labeled <- basicCheck(probPreds1It, probPreds, confValue, memo, "1")
  lenLabeled <- length(labeled$id)
  if (lenLabeled == 0) {
    labeled <- basicCheck(probPreds1It, probPreds, confValue, memo, "2")
    lenLabeled <- length(labeled$id)
    if (lenLabeled == 0) {
      labeled <- basicCheck(probPreds1It, probPreds, confValue, memo, "3")
      lenLabeled <- length(labeled$id)
      if (lenLabeled == 0) {
        labeled <- basicCheck(probPreds1It, probPreds, confValue, memo, "4")
      }
    }
  }
  return(labeled)
}

# FlexCon-C2 funtion
flexConC2 <- function(probPreds, probPredsSuperv, confValue) {
  probPreds <- convertProbPreds(probPreds)
  probPredsSuperv <- convertProbPreds(probPredsSuperv)
  probPredsCon <- (probPreds[, 2] >= confValue)
  probPredsSupervCon <- (probPredsSuperv[, 2] >= confValue)
  probPredsCl <- probPreds[, 1]
  probPredsSupervCl <- probPredsSuperv[, 1]
  newSamples <- which((probPredsCon & probPredsSupervCon)
                      & (probPredsCl == probPredsSupervCl))
  if (length(newSamples) == 0) {
    newSamples <- which((probPredsCon | probPredsSupervCon)
                        & (probPredsCl == probPredsSupervCl))
    if (length(newSamples) == 0) {
      newSamples <- which((probPredsCon & probPredsSupervCon)
                          & (probPredsCl != probPredsSupervCl))
      if (length(newSamples)) {
        addRotSuperv <<- TRUE
      }
    }
  }
  return(newSamples)
}


#' @description This function choose the insertion rule.
#'
#' @param data1It A data frame with all samples seted first iteration.
#' @param dataXIt A data frame with remaining samples.
#' @param confValue The confidence rate, it's a threshold to select samples.
#' @param index1It The sample from data1It that will be compared.
#' @param index The sample from dataXIt that will be compared.
#' @param comp The insertion rule to be used in the comparation.
#'
#' @return It calls the correspondent insertion rule.
#'
letCheck <- function(data1It, dataXIt, confValue, index1It, index, comp) {
  switch(comp,
         "1" = {
           return(classCheck(data1It, dataXIt, confValue, index1It, index))
         },
         "2" = {
           return(confCheck(data1It, dataXIt, confValue, index1It, index))
         },
         "3" = {
           return(diffClassCheck(data1It, dataXIt, confValue, index1It, index))
         },
         "4" = {
           return(diffConfCheck(data1It, dataXIt, confValue, index1It, index))
         }
  )
}
