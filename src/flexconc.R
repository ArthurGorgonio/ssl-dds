# Check which samples in dataXIt have equal classes than data1It
# Check in both matrixes if the confidence value are higger than confValue
basicCheck <- function(data1It, dataXIt, confValue, moda, comparation) {
  samplesData <- c()
  pos <- 0
  xId <- c()
  yCl <- c()
  zConfPred <- c()
  lvls <- match(dataXIt$id, data1It$id)
  for (indice in 1:length(lvls)) {
    if (letCheck(data1It, dataXIt, confValue, indice, moda, comparation)) {
      pos <- pos + 1
      xId[pos] <- dataXIt[indice, 3]
      yCl[pos] <- dataXIt[indice, 1]
      zConfPred[pos] <- dataXIt[indice, 2]
    }
  }
  samplesData <- data.frame(cl = yCl, prob = zConfPred, id = xId)
  return(samplesData)
}

letCheck <- function(data1It, dataXIt, confValue, indice, moda, comparation) {
  switch(comparation,
         "1" = {
           return(classCheck(data1It, dataXIt, confValue, indice))
         },
         "2" = {
           return(confCheck(data1It, dataXIt, confValue, indice))
         },
         "3" = {
           return(diffClassesCheck(data1It, dataXIt, confValue, indice, moda))
         },
         "4" = {
           return(diffConfCheck(data1It, dataXIt, confValue, indice, moda))
         }
         )
  return(FALSE)
}

classCheck <- function(data1It, dataXIt, confValue, indice) {
  if ((as.character(data1It[lvls[indice], 1])
       == as.character(dataXIt[indice, 1]))) {
    if ((data1It[lvls[indice], 2] >= confValue)
        && (dataXIt[indice, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Check in both matrixes if one of confidences values are higger than confValue
# The class of this samples is select observing the sum of the confidences or choose the most voted class
confCheck <- function(data1It, dataXIt, confValue, indice) {
  if ((as.character(data1It[lvls[indice], 1])
       == as.character(dataXIt[indice, 1]))) {
    if ((data1It[lvls[indice], 2] >= confValue)
        || (dataXIt[indice, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Check in both matrixes if both confidences values are higger than confValue
# The class of this samples is select observing the sum of the confidences or choose the most voted class
diffClassesCheck <- function(data1It, dataXIt, confValue, indice, moda) {
  if ((as.character(data1It[lvls[indice], 1])
       != as.character(dataXIt[indice, 1]))) {
    if ((data1It[lvls[indice], 2] >= confValue)
        && (dataXIt[indice, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Check in both matrixes if one of confidences values are higger than confValue
# The class of this samples is select observing the sum of the confidences or choose the most voted class
diffConfCheck <- function(data1It, dataXIt, confValue, indice, moda) {
  if ((as.character(data1It[lvls[indice], 1])
       != as.character(dataXIt[indice, 1]))) {
    if ((data1It[lvls[indice], 2] >= confValue)
        || (dataXIt[indice, 2] >= confValue)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# FlexCon-C the base algorithm
flexConC <- function(learner, predFunc, minSamplesClass, threshold, method,
                     data, sup) {
  # Initial setup, this is equal in all methods FlexCon-C1 and FlexCon-C2
  form <- as.formula(paste(classe, '~', '.'))
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
  classify <- TRUE
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
    model <- generateModel(learner, form, data, sup)
    probPreds <- generateProbPreds(predFunc, model, data, sup)
    id <- getRealId(data, sup)
    probPreds$id = id
    if (it > 1) {
      switch(method,
             "1" = {
               moda <- storageSum(probPreds, moda)
               newSamples <- flexConC1(probPreds1It, probPreds, confValue, moda)
             },
             "2" = {
               moda <- storageFashion(probPreds, moda)
               newSamples <- flexConC1(probPreds1It, probPreds, confValue, moda)
             },
             "3" = {
               model_superv <- generateModel(learner, form, data, sup)
               probPredsSuperv <- generateProbPreds(predFunc, model_superv,
                                                    data, sup)
               newSamples <- flexConC2(probPreds, probPredsSuperv, confValue)
             }
      )
      } else {
        probPreds1It <- probPreds
        idSamples <- which(probPreds[, 2] >= confValue)
        newSamples <- probPreds[idSamples, ]
      }
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
        oldTrainSetIds <- c(oldTrainSetIds, trainSetIds)
        trainSetIds <- (1:nrow(data))[-sup][newSamples]
        sup <- c(sup, trainSetIds)
        validTtrain <- validTraining(data, trainSetIds, nClass, minSamplesClass)
        classify <- validClassification(validTrain, trainSetIds, oldTrainSetIds,
                                        data, nClass, minSamplesClass)
        if (classify) {
          localAcc <- calcLocalAcc()
          confValue <- newConfidence(localAcc, threshold, confValue)
        }
    } else {
      confValue <- max(probPreds[, 2])
    }
    if ((it == maxIts) || (length(sup) == nrow(data))) {
      break
    }
  }
  return(model)
}

flexConC1 <- function(probPreds1It, probPreds, confValue, moda) {
  labeled <- basicCheck(probPreds1It, probPreds, confValue, moda, "1")
  lenLabeled <- length(labeled$id)
  if (lenLabeled == 0) {
    labeled <- basicCheck(probPreds1It, probPreds, confValue, moda, "2")
    lenLabeled <- length(labeled$id)
    if (lenLabeled == 0) {
      labeled <- basicCheck(probPreds1It, probPreds, confValue, moda, "3")
      lenLabeled <- length(labeled$id)
      if (lenLabeled == 0) {
        labeled <- basicCheck(probPreds1It, probPreds, confValue, moda, "4")
      }
    }
  }
  newSamples <- labeled$id
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
