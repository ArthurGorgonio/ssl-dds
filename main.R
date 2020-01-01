#' @description This function check the actual directory has a subdir called src
#'  if exists it's a new working directory
setWorkspace <- function() {
  files <- c("classifiers.R", "crossValidation.R", "database.R", "flexconc.R",
             "functions.R", "MainCephas.R", "statistics.R", "utils.R", "write.R")
  if ("src" %in% list.dirs(full.names = F)) {
    setwd("src")
  } else if (!all(files %in% list.files())) {
    stop("Some file is missing!\n")
  } else {
    stop("Please move to the right directory!\n")
  }
}

seeds <- c(1, 3, 7, 9, 12, 18, 29, 32, 36, 44, 49, 73, 80, 92, 100, 154, 201,
           273, 310, 374, 435, 559, 623, 828, 945, 3341, 3431, 3581, 4134, 8999)

shuffleClassify <- function(size) {
  typeClassify <- 1:length(baseClassifiers)
  return(sample(typeClassify, size, T))
}


setWorkspace()
scripts <- list.files()
for (scri in scripts) {
  source(scri)
}
rm(scripts, scri)
defines()
meansFlexConC1S <- c()
meansFlexConC1V <- c()
databases <- list.files(path = "../datasets")
ratio <- 0.05
for (dataset in databases) {
  dataName <- strsplit(dataset, ".", T)[[1]][1]
  for (seed in seeds) {
    set.seed(seed)
    originalDB <- readData(dataset)
    epoch <- 0
    ensemble <- list()
    while ((nrow(originalDB$data)) > (originalDB$state)) {
      if (originalDB$processed == 0) {
        typeClassifier <- shuffleClassify(3)
      } else {
        typeClassifier <- shuffleClassify(1)
      }
      allDataL <- getBatch(originalDB, 5000)
      begin <- Sys.time()
      dataL <- holdout(allDataL$class, .75)
      dataTrain <- allDataL[dataL$tr, ]
      dataTest <- allDataL[dataL$ts, ]
      rownames(dataTrain) <- as.character(1:nrow(dataTrain))
      epoch <- epoch + 1
      classifier <- baseClassifiers[typeClassifier]
      myFuncs <- funcType[typeClassifier]
      needUpdate <- which(typeClassifier == 4)
      if (length(needUpdate)) {
        classifier[needUpdate] <- attKValue(dataTrain)
      }
      folds <- stratifiedKFold(dataTrain, dataTrain$class)
      for (learner in classifier) {
        trainedModels <- c()
        accFold <- c()
        fmeasureFold <- c()
        precisionFold <- c()
        recallFold <- c()
        accTestDB <- c()
        fmeasureTestDB <- c()
        precisionTestDB <- c()
        recallTestDB <- c()
        for (fold in folds) {
          train <- dataTrain[-fold, ]
          test <- dataTrain[fold, ]
          trainIds <- holdout(train$class, ratio)
          labelIds <- trainIds$tr
          data <- newBase(train, labelIds)
          classDist <- ddply(data[labelIds, ], ~class, summarise,
                             samplesClass = length(class))
          initialAcc <- supAcc(learner@func, data[labelIds, ])
          model <- flexConC(learner, myFuncs[match(list(learner), classifier)],
                            classDist, initialAcc, "1", data, labelIds,
                            learner@func, 5)
          model <- supModel(learner@func, dataTrain)
          trainedModels[[length(trainedModels) + 1]] <- model
          cmFold <- confusionMatrix(model, test)
          accFold <- c(accFold, getAcc(cmFold))
          fmeasureFold <- c(fmeasureFold, fmeasure(cmFold))
          precisionFold <- c(precisionFold, precision(cmFold))
          recallFold <- c(recallFold, recall(cmFold))
          cmTestDB <- confusionMatrix(model, dataTest)
          accTestDB <- c(accTestDB, getAcc(cmTestDB))
          fmeasureTestDB <- c(fmeasureTestDB, fmeasure(cmTestDB))
          precisionTestDB <- c(precisionTestDB, precision(cmTestDB))
          recallTestDB <- c(recallTestDB, recall(cmTestDB))
        }
        end <- Sys.time()
        fileName <- paste(dataName, "txt", sep = ".")
        writeArchive(fileName, "../results/", dataName, accFold, fmeasureFold,
                     precisionFold, recallFold, begin, end, epoch)
        writeArchive(fileName, "../results/", dataName, accTestDB,
                     fmeasureTestDB, precisionTestDB, recallTestDB, begin, end,
                     epoch)
        if (epoch > 1) {
          bestOracle <- trainedModels[[which.max(accTestDB)]]
          realClass <- dataTrain$class
          dataTrain$class <- predictClass(bestOracle, dataTrain)
          ensemblePred <- predictEnsemble(ensemble, dataTrain, nrow(classDist))
          acc <- getAcc(table(ensemblePred, dataTrain$class))
          if (acc < 70) {
            clAcc <- measureEnsemble(ensemble, dataTrain)
            ensemble <- swapEnsemble(ensemble, dataTrain, trainedModels, accTestDB)
            changed <- T
          } else {
            changed <- F
          }
        } else {
          ensemble <- addingEnsemble(ensemble, trainedModels, accTestDB)
        }
      }
    }
  }
}
