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
method <- 1
databases <- list.files(path = "../datasets")
ratio <- 0.1
learner <- baseClassifiers
myFuncs <- funcType

set.seed(19)
for (dataset in databases) {
  begin <- Sys.time()
  dataName <- strsplit(dataset, ".", T)[[1]][1]
  originalDB <- read.arff(paste("../datasets", dataset, sep = "/"))
  originalDB$class <- droplevels(originalDB$class)
  dataL <- holdout(originalDB$class, .9)
  dataTrain <- originalDB[dataL$tr, ]
  dataTest <- originalDB[dataL$ts, ]
  folds <- stratifiedKFold(dataTrain, dataTrain$class)
  visao <- criar_visao(dataTrain)
  dat1 <- visao[[1]]
  dat2 <- visao[[2]]
  
  acc_co <- c()
  acc_co_v1 <- c()
  acc_co_v2 <- c()
  
  fscore_co <- c()
  fscore_co_v1 <- c()
  fscore_co_v2 <- c()
  
  preci_co <- c()
  preci_co_v1 <- c()
  preci_co_v2 <- c()
  
  recall_co <- c()
  recall_co_v1 <- c()
  recall_co_v2 <- c()
  

  for (fold in folds) {
    
    train1 <- dat1[-fold, ]
    test1 <- dat1[fold, ]
    
    train2 <- dat2[-fold, ]
    test2 <- dat2[fold, ]
    
    trainIds <- holdout(train1$class, ratio, mode = "random")
    labelIds <- trainIds$tr
    data1 <- newBase(train1, labelIds)
    data2 <- newBase(train2, labelIds)
    classDist <- ddply(train1[, ], ~class, summarise,
                       samplesClass = length(class))
    co_training <- coTrainingOriginal(learner, myFuncs, data1, data2)
    
    # cmFold <- confusionMatrix(model, test)
    # accFold <- c(accFold, getAcc(cmFold))
    # fmeasureFold <- c(fmeasureFold, fmeasure(cmFold))
    # precisionFold <- c(precisionFold, precision(cmFold))
    # recallFold <- c(recallFold, recall(cmFold))
    # cmTestDB <- confusionMatrix(model, dataTest)
    # accTestDB <- c(accTestDB, getAcc(cmTestDB))
    # fmeasureTestDB <- c(fmeasureTestDB, fmeasure(cmTestDB))
    # precisionTestDB <- c(precisionTestDB, precision(cmTestDB))
    # recallTestDB <- c(recallTestDB, recall(cmTestDB))
    
    cm1 <- confusionMatrix(co_training[[1]], dataTest)
    cm2 <- confusionMatrix(co_training[[2]], dataTest)
    # Acurácia
    acc_model1 <- getAcc(cm1)
    acc_model2 <- getAcc(cm2)
    acc_model_mean <- mean(c(acc_model1, acc_model2))
    acc_co <- c(acc_co, acc_model_mean)
    acc_co_v1 <- c(acc_co_v1, acc_model1)
    acc_co_v2 <- c(acc_co_v2, acc_model2)
    
    # Fscore
    fscore_model1 <- fmeasure(cm1)
    fscore_model2 <- fmeasure(cm2)
    fscore_model_mean <- mean(c(fscore_model1, fscore_model2))
    fscore_co <- c(fscore_co, fscore_model_mean)
    fscore_co_v1 <- c(fscore_co_v1, fscore_model1)
    fscore_co_v2 <- c(fscore_co_v2, fscore_model2)
    
    # Precision
    preci_model1 <- precision(cm1)
    preci_model2 <- precision(cm2)
    preci_model_mean <- mean(c(preci_model1, preci_model2))
    preci_co <- c(preci_co, preci_model_mean)
    preci_co_v1 <- c(preci_co_v1, preci_model1)
    preci_co_v2 <- c(preci_co_v2, preci_model2)
    
    # recall
    recall_model1 <- recall(cm1)
    recall_model2 <- recall(cm2)
    recall_model_mean <- mean(c(recall_model1, recall_model2))
    recall_co <- c(recall_co, recall_model_mean)
    recall_co_v1 <- c(recall_co_v1, recall_model1)
    recall_co_v2 <- c(recall_co_v2, recall_model2)
    
    cat("Modelo 1 (%)\nAcurácia =\t", acc_model1, "\nF-Score =\t", fscore_model1,
        "\nPrecisão =\t", preci_model1, "\nRecall =\t", recall_model1, "\n\n")
    
    cat("Modelo 2 (%)\nAcurácia =\t", acc_model2, "\nF-Score =\t", fscore_model2,
        "\nPrecisão =\t", preci_model2, "\nRecall =\t", recall_model2, "\n\n")
    
    cat("Modelo Médio (%)\nAcurácia =\t", acc_model_mean, "\nF-Score =\t", fscore_model_mean,
        "\nPrecisão =\t", preci_model_mean, "\nRecall =\t", recall_model_mean, "\n\n")
    
    
  }
  end <- Sys.time()
  writeArchive("coTrainingMedia.txt", "./", dataName, acc_co, fscore_co,
               preci_co, recall_co, begin, end, match(dataset, databases))
  writeArchive("coTrainingVisao1.txt", "./", dataName, acc_co_v1, fscore_co_v1,
               preci_co_v1, recall_co_v1, begin, end, match(dataset, databases))
  writeArchive("coTrainingVisao2.txt", "./", dataName, acc_co_v2, fscore_co_v2,
               preci_co_v2, recall_co_v2, begin, end, match(dataset, databases))
  
}

