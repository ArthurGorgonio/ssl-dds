#' @description This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/workspace/ssl-dds/src")
  } else {
    setwd("C:\\Projects\\Mestrado")
  }
}
msgs = c("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
       "\n3 - JRip\n4 - IBk")
args = commandArgs(trailingOnly = TRUE)
if ((args == "-h") || (args == "--help")) {
  cat(msgs)
} else if ((as.integer(args) == F) || (is.na(as.integer(args))) ||
           (as.integer(args) > 4) || (as.integer(args) < 1)) {
  stop(msgs)
} else {
  setWorkspace()
  as.integer(args)
  scripts <- list.files()
  for (scri in scripts) {
    source(scri)
  }
  defines()
  meansFlexConC1S <- c()
  meansFlexConC1V <- c()
  databases <- list.files(path = "../bases")
  for (dataset in databases) {
    originalDB <- getDataset(dataset)
    kNN
  }
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
