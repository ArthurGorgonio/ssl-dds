#' @description This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/workspace/ssl-dds/")
  } else {
    setwd("C:\\Projects\\Mestrado")
  }
}
msgs = "The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
       "\n3 - JRip\n4 - IBk"
args = commandArgs(trailingOnly = TRUE)
if ((args == "-h") || (args == "--help")) {
  cat(msgs)
} else if ((as.integer(args) == F) || (is.na(as.integer(args))) ||
           (as.integer(args) > 4) || (as.integer(args) < 1)) {
  stop(msgs)
} else {
  setWorkspace()
  cat("Args = ", args)
  source("functions.R")
  source("classifiers.R")
  source("crossValidation.R")
  source("flexconc.R")
  source("utils.R")
  # folds <- stratifiedKFold(data, data$class)
  # for (fold in folds) {
  #   train <- data[-fold, ]
  #   test <- data[fold, ]
  #   model <- naiveBayes(class ~ ., train)
  #   cat(rep("*",15), "\n")
  #   mClass <- predict(model, test[,-1], type = "class")
  #   table(mClass, test$class)
  # }
}
