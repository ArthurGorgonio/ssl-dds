#' @description This function check the actual directory has a subdir called src
#'  if exists it's a new working directory
setWorkspace <- function() {
  files <- c("classifiers.R", "crossValidation.R", "database.R", "flexconc.R",
  "functions.R", "statistics.R", "utils.R", "write.R")
  if ("src" %in% list.dirs(full.names = F)) {
    setwd("src")
  } else if (!all(files %in% list.files())) {
    stop("Some file is missing!\n")
  } else {
    stop("Please move to the right directory!\n")
  }
}

# setWorkspace()
scripts <- list.files()
for (scri in scripts) {
  source(scri)
}
rm(scripts, scri)
defines()
meansFlexConC1S <- c()
databases <- list.files(path = "../datasets")
ratio <- 0.1
set.seed(19)
for (dataset in databases) {
  begin <- Sys.time()
  dataName <- strsplit(dataset, ".", T)[[1]][1]
  originalDB <- read.arff(paste("../datasets", dataset, sep = "/"))
  originalDB$class <- droplevels(originalDB$class)
  dataL <- holdout(originalDB$class, .9)
  dataTrain <- originalDB[dataL$tr, ]
  dataTest <- originalDB[dataL$ts, ]
  learner <- baseClassifiers
  myFuncs <- funcType
  folds <- stratifiedKFold(dataTrain, dataTrain$class)
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
    trainIds <- holdout(train$class, ratio, mode = "random")
    labelIds <- trainIds$tr
    data <- newBase(train, labelIds)
    classDist <- ddply(train[, ], ~class, summarise,
                       samplesClass = length(class))
    initialAcc <- supAcc(learner@func, data[labelIds, ])
    model <- flexConC(learner, myFuncs, classDist, initialAcc, "1", data,
                      labelIds, learner@func, 5)
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
  writeArchive("TestingUsingFolds.txt", "../results/", dataName, accFold, 
               fmeasureFold, precisionFold, recallFold, begin, end)
  writeArchive("TestingUsingTestDB.txt", "../results/", dataName, accTestDB, 
               fmeasureTestDB, precisionTestDB, recallTestDB, begin, end)
}

