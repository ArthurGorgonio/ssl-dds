#' @description This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/workspace/ssl-dds/src")
  } else {
    stop("Setup right directory!\n")
  }
}

msgs = c("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - JRip",
       "\n3 - rpartXse\n4 - IBk")
args = commandArgs(trailingOnly = TRUE)

if ((args == "-h") || (args == "--help")) {
  cat(msgs)
} else if ((as.integer(args) == F) || (is.na(as.integer(args))) ||
           (as.integer(args) > 4) || (as.integer(args) < 1)) {
  stop(msgs)
} else {
  # args <- "1"
  setWorkspace()
  args <- as.integer(args)
  scripts <- list.files()
  for (scri in scripts) {
    source(scri)
  }
  rm(scripts, scri)
  defines()
  meansFlexConC1S <- c()
  meansFlexConC1V <- c()
  databases <- list.files(path = "../datasets")
  for (dataset in databases) {
    originalDB <- readData(dataset)
#' TODO iterate in `originalDB` and for each iteration generate a batch with min(remainSamples, batchSize) 
    dataL <- getBatch(originalDB, 500)
    attKValue(dataL)
    folds <- stratifiedKFold(dataL, dataL$class)
    for (fold in folds) {
      train <- dataL[-fold, ]
      test <- dataL[fold, ]
      
    }
  }
  #   model <- naiveBayes(class ~ ., train)
  #   cat(rep("*",15), "\n")
  #   mClass <- predict(model, test[,-1], type = "class")
  #   table(mClass, test$class)
  # }
}
