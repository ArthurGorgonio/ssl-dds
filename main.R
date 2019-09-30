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
  typeClassify <- 1:length(obj)
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
# learner <- myLearner[[1]]
# fold <- folds[[1]]
# predFunc <- myFuncs[match(list(learner), myLearner)]
# method <- "1"
# sup <- labelIds
# classiName <- learner@func
# cr <- 5
# ##
for (iniLab in 1:5) {
  ratio <- iniLab * 0.05
  for (dataset in databases) {
    originalDB <- readData(dataset)
    while (!(originalDB$finished)) {
      if (originalDB$processed == 0) {
        classify <- shuffleClassify(3)
      } else {
        classify <- shuffleClassify(1)
      }
      myLearner <- obj[classify]
      myFuncs <- funcType[classify]
      dataL <- getBatch(originalDB, 500)
      needUpdate <- which(match(myLearner, obj) == 4)
      if (length(needUpdate)) {
        myLearner[needUpdate] <- attKValue(dataL)
      }
      #'
      #' Camp fire
      #' Frost fall
      #' iNeed
      #'
      folds <- stratifiedKFold(dataL, dataL$class)
      for (learner in myLearner) {
        for (fold in folds) {
          train <- dataL[-fold, ]
          test <- dataL[fold, ]
          trainIds <- holdout(train$class, ratio)
          labelIds <- trainIds$tr
          data <- newBase(train, labelIds)
          classDist <- ddply(data[labelIds, ], ~class, summarise,
                             samplesClass = length(class))
          initialAcc <- supAcc(learner@func, data[labelIds, ])
          model <- flexConC(learner, myFuncs[match(list(learner), myLearner)],
                            classDist, initialAcc, "1", data, labelIds,
                            learner@func, 5)
          modelSup <- supModel(learner@func, train)
          confusionMatrix(model, test)  
          confusionMatrix(modelSup, test)  
          getAcc(confusionMatrix(model, test))
          getAcc(confusionMatrix(modelSup, test))
        }
        #' TODO below checklist:
        #'  [X] Split the `train` into label and unlabel sets.
        #'  [X] Call training script to create the ensemble (3 instances).
        #'  [?] Add more each iteration of the method, if needed.
        #'  [ ] Also meansure the accuracy of each individual classifier.
        #'  [ ] Compare with oracle ensemble member.
        #'  [ ] Storage all ten model per learner.
        #'  [ ] Measure the best model, using Acc.
      }
    }
  }
}
