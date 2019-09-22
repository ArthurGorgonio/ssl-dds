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
  return(sample(typeClassify, size))
}

setWorkspace()
scripts <- list.files()
for (scri in scripts) {
  source(scri)
}
rm(scripts, scri, msgs)
defines()
myFuncs <- funcType[as.integer(args)]
meansFlexConC1S <- c()
meansFlexConC1V <- c()
databases <- list.files(path = "../datasets")
for (iniLab in 1:5) {
  ratio <- iniLab * 0.05
  for (dataset in databases) {
    originalDB <- readData(dataset)
    while (!(originalDB$finished)) {
      if (originalDB$processed == 0) {
        classify <- shuffleClassify(3)
        myLearner <- obj[classify]
        #' TODO length(myLearner) return 3 - classifiers trained with FlexCon-C
      } else {
        classify <- shuffleClassify(1)
        #' 1 classifier trained with FlexCon-C
      }
      dataL <- getBatch(originalDB, 500)
      attKValue(dataL)
      folds <- stratifiedKFold(dataL, dataL$class)
      for (fold in folds) {
        train <- dataL[-fold, ]
        test <- dataL[fold, ]
        allIds <- holdout(train$class, ratio = ((iniLab * 5) / 100))
        labelIds <- allIds$tr
        initialAcc <- supAcc()
        data <- newBase(train, labelIds)
        
        #' TODO below checklist:
        #'  [X] Split the `train` into label and unlabel sets
        #'  [ ] Call training script to create the ensemble (3 instances)
        #'  [ ] Add more each iteration of the method, if needed.
        #'  [ ] Also meansure the accuracy of each individual classifier
        #'  [ ] Compare with oracle ensemble member.
      }
    }
  }
}
