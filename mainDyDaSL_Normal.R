args <- commandArgs(TRUE)
# args <- c("-s", "1", "-l", "100")

#' @description This function check the actual directory has a sub directory
#'  called src if exists it's a new working directory
setWorkspace <- function() {
  files <- c("classifiers.R", "crossValidation.R", "database.R", "flexconc.R",
             "functions.R", "statistics.R", "utils.R", "write.R")
  if ("src" %in% list.dirs(full.names = F)) {
    setwd("src")
  } else if (all(files %in% list.files())) {
    print("All files exists!")
  } else {
    stop("The follow file(s) are missing!\n", files[!files %in% list.files()])
  }
}

options(java.parameters = "-Xmx4g")

shuffleClassify <- function(size) {
  typeClassify <- 1:length(baseClassifiers)
  return(sample(typeClassify, size, T))
}


setWorkspace()
source('utils.R')
installNeedPacks()
token <- fromJSON('../token.txt')
pbSetup(token$key, defdev = 1)

scripts <- list.files(pattern='*.R', recursive=T)
for (scri in scripts) {
  source(scri)
}
path <- "../results/detailed"
rm(scripts, scri)
databases <- list.files(path = "../datasets/")
myParam <- atribArgs(args, databases)
ratios <- myParam$ratios
lengthBatch <- myParam$lengthBatch
# lengthBatch <- c(100, 250, 750, 500, 1000, 2500, 5000)
databases <- databases[myParam$iniIndex:myParam$finIndex]
defines()

ratio <- 0.1
for (dataLength in lengthBatch) {
  kValue <- floor(sqrt(dataLength))
  for (dataset in databases) {
    dataName <- strsplit(dataset, ".", T)[[1]][1]
    script_name <- "_mainDyDaSL_Normal_"
    fileName <- paste(ratio * 100, dataName, script_name, dataLength, ".txt", sep = "")
    title <- paste("test", fileName, sep = "")
    headerDetailedOutputEnsemble(title, path, dataName, "DyDaSL - Simple Vote")
    cat(dataName)
    calculate <- TRUE
    set.seed(19)
    ensemble <- list()
    ensemble_weights <- c()
    it <- 0
    typeClassifier <- shuffleClassify(10)
    train <- readData(dataset, path = "../datasets/")
    all_classes <- sort(levels(train$data$class))
    totalInstances <- nrow(train$data)
    while (totalInstances > (train$state)) {
      detect_drift <- FALSE
      begin <- Sys.time()
      it <- it + 1
      batch <- getBatch(train, dataLength)
      # batch$class <- droplevels(batch$class)
      cat("Foram processadas: ", train$processed, "/", totalInstances, "\t")
      rownames(batch) <- as.character(1:nrow(batch))
      batchIds <- holdout(batch$class, ratio, seed = 1, mode="random")
      batchLabeled <- batchIds$tr
      rm(batchIds)
      data <- newBase(batch, batchLabeled)
      data$class <- droplevels(data$class)
      if (((totalInstances - (train$state)) > 100) && 
          (length(levels(data$class)) > 1)) {
        classDist <- ddply(data[batchLabeled, ], ~class, summarise,
                           samplesClass = length(class))
        if (it > 1) {
          # ensemble <- knora(valid_base_classifier, data[batchLabeled], 
                            # sort(levels(batch$class)))
          ensemblePred <- predictEnsemble(ensemble, ensemble_weights,
                                          data[batchLabeled, ],
                                          all_classes)
          cmLabeled <- table(ensemblePred, data[batchLabeled, ]$class)
          cmLabeled <- fixCM(cmLabeled, all_classes)
          ensembleAcc <- getAcc(cmLabeled)
          cat("Accuracy Ensemble:\t", ensembleAcc, "\n")
          if (calculate) {
            calculate <- FALSE
            acceptabelAcc <- round(ensembleAcc, 2)
          }
          if (ensembleAcc < acceptabelAcc * 0.99) {
            detect_drift <- TRUE
            typeClassifier <- shuffleClassify(1)
            learner <- baseClassifiers[[typeClassifier]]
            initialAcc <- supAcc(learner, data[batchLabeled, ])
            oracle <- flexConC(learner, funcType[typeClassifier], classDist,
                              initialAcc, "1", data, batchLabeled,
                              learner@func)
            oracle_data <- cbind(batch[, -match(label, colnames(batch))],
                                 class=predictClass(oracle, batch))
            ensemble <- swapEnsemble(ensemble, oracle_data, oracle, all_classes)
            calculate <- TRUE
          }
        } else {
          for (i in typeClassifier) {
            learner <- baseClassifiers[[i]]
            initialAcc <- supAcc(learner, data[batchLabeled, ])
            cat(learner@func, '\n')
            model <- flexConC(learner, funcType[i], classDist, initialAcc,
                              "1", data, batchLabeled, learner@func)
            ensemble <- addingEnsemble(ensemble, model)
          } # END FOR
          ensemble_weights <- rep(1, length(ensemble))
        } # END ELSE
      } # END ELSE
      end <- Sys.time()
      ensemble_pred <- predictEnsemble(ensemble, ensemble_weights, batch,
                                       all_classes)
      cm_ensemble <- table(ensemble_pred, batch$class)
      cm_ensemble <- fixCM(cm_ensemble, all_classes)
      cat("\n\tCM TEST:\n")
      # print(cm_ensemble)
      # print(cm_ensemble_weight)
      detailedOutputEnsemble(title, path, length(ensemble), sum(diag(cm_ensemble)), 
                             sum(cm_ensemble) - sum(diag(cm_ensemble)),
                             getAcc(cm_ensemble), fmeasure(cm_ensemble),
                             kappa(cm_ensemble), detect_drift, train$state,
                             difftime(end, begin, units = "mins"))
    } # END WHILE
  } # END FOR DATASETS
} # END FOR BATCHSIZE
