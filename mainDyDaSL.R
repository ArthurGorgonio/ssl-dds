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
rm(scripts, scri)
databases <- list.files(path = "../datasets/")[2]
myParam <- atribArgs(args, databases)
ratios <- myParam$ratios
seeds <- myParam$seeds
lengthBatch <- myParam$lengthBatch
databases <- databases[myParam$iniIndex:myParam$finIndex]
# for (ratio in ratios) {
  ratio <- 0.1
  # for (dataLength in lengthBatch) {
    dataLength <- 500
    kValue <- floor(sqrt(dataLength))
    defines(kValue)
    for (dataset in databases) {
      dataName <- strsplit(dataset, ".", T)[[1]][1]
      cat(dataName)
      epoch <- 0
      # for (seed in seeds) {
        calculate <- TRUE
        epoch <- epoch + 1
        cat("\n\n\nRODADA: ", epoch, "\n\n\n\n")
        set.seed(seed)
        originalDB <- getDatabase(dataset, path = "../datasets/")
        trainTest <- holdout(originalDB$class, .9, mode = "random", seed = 1)
        dataTrain <- originalDB[trainTest$tr, ]
        all_levels <- sort(levels(dataTrain$class))
        folds <- stratifiedKFold(dataTrain, dataTrain$class)
        dataTest <- originalDB[trainTest$ts, ]
        rm(originalDB, trainTest)
        cl <- match(label, colnames(dataTest))
        begin <- Sys.time()
        accTest <- c()
        fmeasureTest <- c()
        precisionTest <- c()
        recallTest <- c()
        accTestEnsemble <- c()
        fmeasureTestEnsemble <- c()
        precisionTestEnsemble <- c()
        recallTestEnsemble <- c()
        for (fold in folds) {
          newTrainDataset <- fixDataset(dataTrain, fold)
          ensemble <- list()
          it <- 0
          typeClassifier <- shuffleClassify(10)
          train <- datastream_dataframe(data = newTrainDataset)
          totalInstances <- nrow(train$data)
          while (totalInstances > (train$state)) {
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
              ensemblePred <- predictEnsemble(ensemble, data[batchLabeled, ],
                                              all_levels)
              cmLabeled <- table(ensemblePred, data[batchLabeled, ]$class)
              ensembleAcc <- getAcc(cmLabeled)
              cat("Accuracy Ensemble:\t", ensembleAcc, "\n")
              if (calculate) {
                calculate <- FALSE
                acceptabelAcc <- round(ensembleAcc, 2)
              }
              if (ensembleAcc < acceptabelAcc) {
                typeClassifier <- shuffleClassify(1)
                learner <- baseClassifiers[[typeClassifier]]
                initialAcc <- supAcc(learner, data[batchLabeled, ])
                oracle <- flexConC(learner, funcType[typeClassifier], classDist,
                                  initialAcc, "1", data, batchLabeled,
                                  learner@func)
                oraclePred <- predictClass(oracle, batch)
                ensemble <- swapEnsemble(ensemble, dataTrain, oracle)
                calculate <- TRUE
              }
            } else {
              for (i in typeClassifier) {
                learner <- baseClassifiers[[i]]
                initialAcc <- supAcc(learner, data[batchLabeled, ])
                model <- flexConC(learner, funcType[i], classDist, initialAcc,
                                  "1", data, batchLabeled, learner@func)
                ensemble <- addingEnsemble(ensemble, model)
              }
            }
          }
          ensemblePred <- predictEnsemble(ensemble, dataTest, all_levels)
          cmEnsemble <- table(ensemblePred, dataTest$class)
          if (length(rownames(cmEnsemble)) != length(colnames(cmEnsemble))) {
            cmEnsemble <- fixCM(cmEnsemble)
          }
          accTestEnsemble <- c(accTestEnsemble, getAcc(cmEnsemble))
          fmeasureTestEnsemble <- c(fmeasureTestEnsemble, fmeasure(cmEnsemble))
          precisionTestEnsemble <- c(precisionTestEnsemble, precision(cmEnsemble))
          recallTestEnsemble <- c(recallTestEnsemble, recall(cmEnsemble))
          cat("\n\tCM TEST:\n")
          print(cmEnsemble)
        }
        end <- Sys.time()
        fileName <- paste(ratio * 100, "%DyDaSL", dataLength, ".txt", sep = "")
        writeArchive(paste("test", fileName, sep = ""), "../results/", dataName,
                     "DyDaSL", accTestEnsemble, fmeasureTestEnsemble,
                     precisionTestEnsemble, recallTestEnsemble, begin, end,
                     epoch)
      # }
    }
#   }
# }
