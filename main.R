#' @description This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/workspace/ssl-dds/src")
  } else {
    stop("Setup right directory!\n")
  }
}


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
for (iniLab in 1:5) {
  #' iniLab <- 1
  ratio <- iniLab * 0.05
  for (dataset in databases) {
    #' dataset <- databases[1]
    originalDB <- readData(dataset)
    while (!(originalDB$finished)) {
      #' TODO count the number of samples in each class, after this use a 
      #' percentage (i.e. 10%) of the samples in minoritary class to be a
      #' threshold to call the validClassify function
      <- ddply(data, ~class, summarise, samplesClass = length(class))
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
      #' fold <- folds[[1]]
      for (fold in folds) {
        train <- dataL[-fold, ]
        test <- dataL[fold, ]
        allIds <- holdout(train$class, ratio)
        labelIds <- allIds$tr
        data <- newBase(train, labelIds)
        #' learner <- myLearner[[1]]
        for (learner in myLearner) {
          initialAcc <- supAcc(learner, dataL[labelIds, ])
          model <- flexConC(learner, myFuncs[match(list(learner), myLearner)], 
                            initialAcc,
                            "1", data, labelIds, learner@func, 5)
        }

        #' TODO below checklist:
        #'  [X] Split the `train` into label and unlabel sets.
        #'  [X] Call training script to create the ensemble (3 instances).
        #'  [ ] Add more each iteration of the method, if needed.
        #'  [ ] Also meansure the accuracy of each individual classifier.
        #'  [ ] Compare with oracle ensemble member.
      }
    }
  }
}
