# args <- commandArgs(TRUE)
# args <- c("-s", "9", "-e", "9", "-l", "5000", "-r", "0.1")

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
databases <- list.files(path = "../datasets/")[c(4, 10, 11)]
myParam <- atribArgs(args, databases)
ratios <- myParam$ratios
lengthBatch <- 100
databases <- databases[myParam$iniIndex:myParam$finIndex]
# for (ratio in ratios) {
  ratio <- 0.1
  for (dataLength in lengthBatch) {
    kValue <- floor(sqrt(dataLength))
    defines()
    for (dataset in databases) {
      dataName <- strsplit(dataset, ".", T)[[1]][1]
      fileName <- paste(ratio * 100, dataName, dataLength, ".txt", sep = "")
      title <- paste("test", fileName, sep = "")
      headerDetailedOutputEnsemble(title, path, dataName, "DyDaSL")
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
        batch$class <- droplevels(batch$class)
        cat("Foram processadas: ", train$processed, "/", totalInstances, "\t")
        rownames(batch) <- as.character(1:nrow(batch))
        batchIds <- holdout(batch$class, ratio, seed = 1, mode="random")
        batchLabeled <- batchIds$tr
        rm(batchIds)
        data <- newBase(batch, batchLabeled)
        data$class <- droplevels(data$class)
        classDist <- ddply(data[batchLabeled, ], ~class, summarise,
                           samplesClass = length(class))
        if (it > 1) {
          # ensemble <- knora(valid_base_classifier, data[batchLabeled], 
                            # sort(levels(batch$class)))
          ensemblePred <- predictEnsemble(ensemble, data[batchLabeled, ],
                                          sort(levels(batch$class)))
          cmLabeled <- table(ensemblePred, data[batchLabeled, ]$class)
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
        end <- Sys.time()
        ensemble_pred <- predictEnsemble(ensemble, batch, all_classes)
        ensemble_weights <- weightEnsemble(ensemble, batch, all_classes)
        ensemble_pred_weighted <- predictEnsembleConfidence(ensemble, 
                                                            ensemble_weights,
                                                            batch, all_classes)
        cm_ensemble <- table(ensemble_pred, batch$class)
        cm_ensemble_weight <- table(ensemble_pred_weighted, batch$class)
        if (length(rownames(cm_ensemble)) != length(colnames(cm_ensemble))) {
          cm_ensemble <- fixCM(cm_ensemble)
        }
        if (length(rownames(cm_ensemble_weight)) !=
            length(colnames(cm_ensemble_weight))) {
          cm_ensemble_weight <- fixCM(cm_ensemble_weight)
        }
        cat("\n\tCM TEST:\n")
        # print(cm_ensemble)
        # print(cm_ensemble_weight)
        detailedOutputEnsemble_debug(title, path, length(ensemble), sum(diag(cm_ensemble)), 
                               sum(cm_ensemble) - sum(diag(cm_ensemble)),
                               getAcc(cm_ensemble), fmeasure(cm_ensemble),
                               kappa(cm_ensemble), sum(diag(cm_ensemble_weight)), 
                               sum(cm_ensemble_weight) - sum(diag(cm_ensemble_weight)),
                               getAcc(cm_ensemble_weight), fmeasure(cm_ensemble_weight),
                               kappa(cm_ensemble_weight), detect_drift, train$state,
                               difftime(end, begin, units = "mins"))
        } # END WHILE
      } # END FOR DATASETS
    } # END FOR BATCHSIZE
# } # END FOR RATIOS