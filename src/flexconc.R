# Check which samples in dataXIt have equal classes than data1It
# Check in both matrixes if the confidence value are higger than confValue
classCheck <- function(data1It, dataXIt, confValue) {
  examples <- c()
  pos <- 0
  xId <- c()
  yCl <- c()
  zConfPred <- c()
  lvls <- match(dataXIt$id, data1It$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data1It[lvls[indice], 1])
         == as.character(dataXIt[indice, 1]))) {
      if ((data1It[lvls[indice], 2] >= confValue)
          && (dataXIt[indice, 2] >= confValue)) {
        pos <- pos + 1
        xId[pos] <- indice
        yCl[pos] <- dataXIt[indice, 1]
        zConfPred[pos] <- dataXIt[indice, 2]
      }
    }
  }
  examples <- data.frame(id = xId, cl = yCl)
  return(examples)
}

# Check in both matrixes if one of confidences values are higger than confValue
# The class of this samples is select observing the sum of the confidences or choose the most voted class
confidenceCheck <- function(data1It, dataXIt, confValue) {
  examples <- c()
  pos <- 0
  xId <- c()
  yCl <- c()
  zConfPred <- c()
  lvls <- match(dataXIt$id, data1It$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data1It[lvls[indice], 1])
         == as.character(dataXIt[indice, 1]))) {
      if ((data1It[lvls[indice], 2] >= confValue)
          || (dataXIt[indice, 2] >= confValue)) {
        pos <- pos + 1
        xId[pos] <- indice
        yCl[pos] <- dataXIt[indice, 1]
        zConfPred[pos] <- dataXIt[indice, 2]
      }
    }
  }
  examples <- data.frame(id = xId, cl = yCl)
  return(examples)
}

# Check in both matrixes if both confidences values are higger than confValue
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentClassesCheck <- function(data1It, dataXIt, confValue, moda) {
  pos <- 0
  xId <- c()
  yCl <- c()
  zConfPred <- c()
  lvls <- match(dataXIt$id, data1It$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data1It[lvls[indice], 1])
         != as.character(dataXIt[indice, 1]))) {
      if ((data1It[lvls[indice], 2] >= confValue)
          && (dataXIt[indice, 2] >= confValue)) {
        pos <- pos + 1
        xId[pos] <- indice
        yCl[pos] <- searchClass(xId[pos], moda)
        zConfPred[pos] <- dataXIt[indice, 2]
      }
    }
  }
  examples <- data.frame(id = xId, cl = yCl)
  return(examples)
}

# Check in both matrixes if one of confidences values are higger than confValue
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentConfidencesCheck <- function(data1It, dataXIt, confValue, moda) {
  pos <- 0
  xId <- c()
  yCl <- c()
  zConfPred <- c()
  lvls <- match(dataXIt$id, data1It$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data1It[lvls[indice], 1])
         != as.character(dataXIt[indice, 1]))) {
      if ((data1It[lvls[indice], 2] >= confValue)
          || (dataXIt[indice, 2] >= confValue)) {
        pos <- pos + 1
        xId[pos] <- indice
        yCl[pos] <- searchClass(xId[pos], moda)
        zConfPred[pos] <- dataXIt[indice, 2]
      }
    }
  }
  examples <- data.frame(id = xId, cl = yCl)
  return(examples)
}

# FlexCon-C the base algorithm
flexConC <- function(learner, predFunc, minSamplesClass, limiar, method) {
  # Initial setup, this is equal in all methods FlexCon-C1 and FlexCon-C2
  form <- as.formula(paste(classe, '~', '.'))
  data <- base
  confValue <- 0.95
  maxIts <- 100
  verbose <- TRUE
  it <- 0
  nSamplesClass <- ddply(data, ~class, summarise, samplesClass = length(class))
  nClass <- NROW(nSamplesClass) - 1
  lenLabeled <- 0
  totalLab <- 0
  trainSet <<- c()
  validTrain <<- FALSE
  classificar <- TRUE
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  trainSetIds <- c()
  oldTrainSetIds <- c()
  # FlexCon-C1 only
  if ((method == "1") || (method == "2")) {
    moda <- matrix(data = rep(0, length(originalDB$class)),
                   ncol = length(levels(originalDB$class)),
                   nrow = NROW(originalDB), byrow = TRUE,
                   dimnames = list(row.names(originalDB),
                   sort(levels(originalDB$class), decreasing = FALSE)))
  }
  # FlexCon-C2 only
  addRotSuperv <- FALSE
  repeat {
    newSamples <- c()
    correct <- 0
    it <- it + 1
    if (lenLabeled > 0) {
      lenLabeled <- 0
      validTtrain <- validTraining(data, trainSetIds, nClass, minSamplesClass)
      classificar <- validClassification(validTrain, trainSetIds,
                                         oldTrainSetIds, data, nClass,
                                         minSamplesClass)
      if (classificar) {
        acc_local <- calcLocalAcc()
        confValue <- newConfidence(acc_local, limiar, confValue)
      }
    }
    model <- generateModel(learner, form, data, sup)
    probPreds <- generateProbPreds(predFunc, model, data, sup)
    switch(method,
            "1" = {
              moda <- storageSum(probPreds, moda)
              newSamples <- flexConC1(probPreds, confValue, moda, it)
            },
            "2" = {
              moda <- storageFashion(probPreds, moda)
              newSamples <- flexConC1(probPreds, confValue, moda, it)
            },
            "3" = {
              model_superv <- generateModel(learner, form, data, sup)
              probPredsSuperv <- generateProbPreds(predFunc, model_superv,
                                                     data, sup)
              newSamples <- flexConC2(probPreds, probPredsSuperv, confValue)
            }
    )
    if (length(newSamples)) {
      newData <- data[(1:nrow(data))[-sup][newSamples], as.character(form[2])]
      if (addRotSuperv) {
        addRotSuperv <- FALSE
        newData <- as.character(probPredsSuperv[newSamples, 1])
      } else {
        newData <- as.character(probPreds[newSamples, 1])
      }
      lenLabeled <- length(newData)
      total_rot <- total_rot + lenLabeled
      correct <- 0
      correct <- (training[(1:nrow(data))[-sup][newSamples],
                           as.character(form[2])] == newData)
      correctLen <- NROW(correct)
      for (w in 1:correctLen) {
        if (correct[w] == TRUE) {
          correct <<- correct + 1
        }
      }
      oldTrainSetIds <- appendVectors(oldTrainSetIds, trainSetIds)
      trainSetIds <- (1:nrow(data))[-sup][newSamples]
      sup <- c(sup, trainSetIds)
    } else {
      confValue <- max(probPreds[ , 2])
    }
    if ((it == maxIts) || ((length(sup) / nrow(data)) >= 1)) {
      break
    }
  }
  return(model)
}

flexConC1 <- function(probPreds, confValue, moda, it) {
  if (it == 1) {
    probPreds1It <<- probPreds
    newSamples <- which(probPreds[ , 2] >= confValue)
    rotulados <- data.frame(id = probPreds[newSamples, 3],
                            cl = probPreds[newSamples, 1])
  } else {
    rotulados <- classCheck(probPreds1It, probPreds, confValue)
    lenLabeled <- length(rotulados$id)
    if (lenLabeled == 0) {
      rotulados <- confidenceCheck(probPreds1It, probPreds, confValue)
      lenLabeled <- length(rotulados$id)
      if (lenLabeled == 0) {
        rotulados <- differentClassesCheck(probPreds1It, probPreds, confValue,
                                           moda)
        lenLabeled <- length(rotulados$id)
        if (lenLabeled == 0) {
          rotulados <- differentConfidencesCheck(probPreds1It, probPreds,
                                                 confValue, moda)
        }
      }
    }
  }
  newSamples <- rotulados$id
  return(newSamples)
}

# FlexCon-C2 funtion
flexConC2 <- function(probPreds, probPredsSuperv, confValue) {
  probPreds <- convertProbPreds(probPreds)
  probPredsSuperv <- convertProbPreds(probPredsSuperv)
  probPredsCon <- (probPreds[, 2] >= confValue)
  probPredsSupervCon <- (probPredsSuperv[, 2] >= confValue)
  probPredsCl <- probPreds[, 1]
  probPredsSupervCl <-  probPredsSuperv[, 1]
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

