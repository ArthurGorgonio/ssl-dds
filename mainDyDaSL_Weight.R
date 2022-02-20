args <- commandArgs(TRUE)
# args <- c("-s", "1", "-e", "1", "-l", "100", "-r", "0.1")

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
scripts <- list.files()
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
# for (ratio in ratios) {
  ratio <- 0.1
  for (dataLength in lengthBatch) {
    kValue <- floor(sqrt(dataLength))
    for (dataset in databases) {
      dataName <- strsplit(dataset, ".", T)[[1]][1]
      script_name <- "_mainDyDaSL_Weight_"
      fileName <- paste(ratio * 100, dataName, script_name, dataLength, ".txt", sep = "")
      title <- paste("test", fileName, sep = "")
      headerDetailedOutputEnsemble(title, path, dataName, "DyDaSL - Weight by Acc")
      cat(dataName)
      epoch <- 0
      calculate <- TRUE
      epoch <- epoch + 1
      cat("\n\n\nRODADA: ", epoch, "\n\n\n\n")
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
            ensemble_weights <- weightEnsemble(ensemble, data[batchLabeled, ],
                                               all_classes)
            ensemble_pred_weighted <- predictEnsembleConfidence(ensemble, 
                                                                ensemble_weights,
                                                                data[batchLabeled, ],
                                                                all_classes)
            cmLabeled <- table(ensemble_pred_weighted, data[batchLabeled, ]$class)
            cmLabeled <- fixCM(cmLabeled, all_classes)
            ensembleAcc <- getAcc(cmLabeled)
            cat("Accuracy Ensemble:\t", ensembleAcc, "\n")
            if (calculate) {
              calculate <- FALSE
              acceptabelAcc <- round(ensembleAcc, 2)
            }
            if (ensembleAcc < acceptabelAcc * 0.99) {
              detect_drift <- TRUE
              train_sucess <- FALSE
              typeClassifier <- shuffleClassify(1)
              learner <- baseClassifiers[[typeClassifier]]
              initialAcc <- supAcc(learner, data[batchLabeled, ])
              oracle <- flexConC(learner, funcType[typeClassifier], classDist,
                                initialAcc, "1", data, batchLabeled,
                                learner@func)
              train_sucess <- TRUE
              oraclePred <- predictClass(oracle, batch)
              ensemble <- swapEnsemble(ensemble, data, oracle)
              calculate <- TRUE
            }
          } else {
            for (i in typeClassifier) {
              learner <- baseClassifiers[[i]]
              initialAcc <- supAcc(learner, data[batchLabeled, ])
              model <- flexConC(learner, funcType[i], classDist, initialAcc,
                                "1", data, batchLabeled, learner@func)
              ensemble <- addingEnsemble(ensemble, model)
            } # END FOR
          } # END ELSE
        } # END ELSE
        end <- Sys.time()
        ensemble_weights <- weightEnsemble(ensemble, batch, all_classes)
        ensemble_pred_weighted <- predictEnsembleConfidence(ensemble, 
                                                            ensemble_weights,
                                                            batch, all_classes)
        cm_ensemble_weight <- table(ensemble_pred_weighted, batch$class)
        cm_ensemble_weight <- fixCM(cm_ensemble_weight, all_classes)
        cat("\n\tCM TEST:\n")
        # print(cm_ensemble)
        # print(cm_ensemble_weight)
        detailedOutputEnsemble(title, path, length(ensemble_weights), 
                               sum(diag(cm_ensemble_weight)), 
                               sum(cm_ensemble_weight) - sum(diag(cm_ensemble_weight)),
                               getAcc(cm_ensemble_weight), fmeasure(cm_ensemble_weight),
                               kappa(cm_ensemble_weight), detect_drift, train$state,
                               difftime(end, begin, units = "mins"))
        } # END WHILE
      } # END FOR DATASETS
    } # END FOR BATCHSIZE
    msg <- paste("Batch Size = ", dataLength, "\nTime: ", Sys.time(), sep = "")
    pbPost("note", "Experiment Finished!!", msg)
# } # END FOR RATIOS
