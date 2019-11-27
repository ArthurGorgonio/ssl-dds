#' @description This function check the actual directory has a subdir called src
#'  if exists it's a new working directory
setWorkspace <- function() {
  if ("src" %in% list.dirs(full.names = F)) {
    setwd("src")
  } else {
    cat("Please move to the right directory!\n")
  }
}

# seeds <- c(1, 49, 31, 435, 14134, 3431, 314, 7, 8999, 559)

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
set.seed(1)
for (iniLab in 1:5) {
  ratio <- iniLab * 0.05
  for (dataset in databases) {
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
      dataL <- holdout(allDataL$class, .8)
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
        accTestDB <- c()
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
          # model <- supModel(learner@func, dataTrain)
          trainedModels[[length(trainedModels) + 1]] <- model
          accFold <- c(accFold, getAcc(confusionMatrix(model, test)))
          accTestDB <- c(accTestDB, getAcc(confusionMatrix(model, dataTest)))
        }
        cat("Classifier = ", learner@func, "\nAcc per Fold = ", accFold,
            "\nAcc in Test = ", accTestDB, "\n\n")
        if (epoch > 1) {
          bestOracle <- trainedModels[which.max(accTestDB)]
          realClass <- dataTrain$class
          dataTrain$class <- predictClass(bestOracle, dataTrain)
          # ensemble predict
          
          clAcc <- measureEnsemble(ensemble, dataTrain)
          ensemble <- swapEnsemble(ensemble, dataTrain, trainedModels, accTestDB)
          #Adjust ensemble
          #' TODO When the ensemble need to be fitted:
          #'  [X] Mensure the oracle and all ensemble;
          #'  [X] Determine a threshold to decides if necessary to change the
          #'       ensemble;
          #'  [ ] Determine the type of change (adding, substitute or remove) a
          #'       model from the ensemble;
          #'       [X] adding;
          #'       [X] remove; strategy the worst classifier
          #'       [~] substitute;
          #'        - [~] worst, but the same type;
          #'        - [X] worst of any type;
        } else {
          ensemble <- addingEnsemble(ensemble, trainedModels, accTestDB)
        }
        #Adjust ensemble
        #' TODO below checklist:
        #'  [X] Split the `train` into label and unlabel sets.
        #'  [X] Call training script to create the ensemble (3 instances).
        #'  [?] Add more each iteration of the method, if needed.
        #'  [X] Also meansure the accuracy of each individual classifier.
        #'  [X] Storage all ten model per learner.
        #'  [X] Measure the best model, using Acc.
        #'  [ ] Compare with oracle ensemble member.
        #'  [X] Join the models in the ensemble list.
      }
    }
  }
}



for (learner in classifier) {
  ensemble[[length(ensemble) + 1]] <- supModel(learner@func, dataTrain)
}
