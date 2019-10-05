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
# # DEFINES!!
# iniLab <- 1
# dataset <- databases[1]
# learner <- classifier[[1]]
# fold <- folds[[1]]
# predFunc <- myFuncs[match(list(learner), classifier)]
# method <- "1"
# sup <- labelIds
# classiName <- learner@func
# cr <- 5
#'
#' Camp fire
#' Frost fall
#' iNeed
#'
# ##
for (iniLab in 1:5) {
  ratio <- iniLab * 0.05
  for (dataset in databases) {
    originalDB <- readData(dataset)
    while (!(originalDB$finished)) {
      if (originalDB$processed == 0) {
        typeClassifier <- shuffleClassify(3)
      } else {
        typeClassifier <- shuffleClassify(1)
      }
      classifier <- baseClassifiers[typeClassifier]
      myFuncs <- funcType[typeClassifier]
      dataL <- getBatch(originalDB, 500)
      needUpdate <- which(match(classifier, baseClassifiers) == 4)
      if (length(needUpdate)) {
        classifier[needUpdate] <- attKValue(dataL)
      }
      folds <- stratifiedKFold(dataL, dataL$class)
      for (learner in classifier) {
        trainedModels <- c()
        accuracy <- c()
        for (fold in folds) {
          train <- dataL[-fold, ]
          test <- dataL[fold, ]
          trainIds <- holdout(train$class, ratio)
          labelIds <- trainIds$tr
          data <- newBase(train, labelIds)
          classDist <- ddply(data[labelIds, ], ~class, summarise,
                             samplesClass = length(class))
          initialAcc <- supAcc(learner@func, data[labelIds, ])
          model <- flexConC(learner, myFuncs[match(list(learner), classifier)],
                            classDist, initialAcc, "1", data, labelIds,
                            learner@func, 5)
          modelSup <- supModel(learner@func, train)
          trainedModels <- c(trainedModels, list(model))
          accuracy <- c(accuracy, getAcc(confusionMatrix(model, test)))
        }
        ensemble <- theBestModel(ensemble, trainedModels, accuracy)
        #' TODO below checklist:
        #'  [X] Split the `train` into label and unlabel sets.
        #'  [X] Call training script to create the ensemble (3 instances).
        #'  [?] Add more each iteration of the method, if needed.
        #'  [X] Also meansure the accuracy of each individual classifier.
        #'  [X] Storage all ten model per learner.
        #'  [X] Measure the best model, using Acc.
        #'  [ ] Compare with oracle ensemble member.
        #'  [ ] Join the models in the ensemble list.
        #'  [ ] 
      }
    }
  }
}
