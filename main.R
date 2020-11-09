args <- commandArgs(TRUE)

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

seeds <- c(1, 3, 7)
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
databases <- list.files(path = "../datasets")
myParam <- atribArgs(args, databases)
ratios <- myParam$ratios
lengthBatch <- myParam$lengthBatch
databases <- databases[myParam$iniIndex:myParam$finIndex]
for (ratio in ratios) {
  for (dataLength in lengthBatch) {
    kValue <- floor(sqrt(dataLength))
    defines(kValue)
    for (dataset in databases) {
      dataName <- strsplit(dataset, ".", T)[[1]][1]
      cat(dataName)
      epoch <- 0
      for (seed in seeds) {
        epoch <- epoch + 1
        cat("\n\n\nRODADA: ", epoch, "\n\n\n\n")
        set.seed(seed)
        originalDB <- getDatabase(dataset)
        trainTest <- holdout(originalDB$class, .75, mode = "random", seed = seed)
        dataTrain <- originalDB[trainTest$tr, ]
        folds <- stratifiedKFold(dataTrain, dataTrain$class)
        dataTest <- originalDB[trainTest$ts, ]
        rm(originalDB, trainTest)
        cl <- match(label, colnames(dataTest))
        begin <- Sys.time()
        accTest <- c()
        fmeasureTest <- c()
        precisionTest <- c()
        recallTest <- c()
        for (fold in folds) {
          ensemble <- list()
          it <- 0
          typeClassifier <- shuffleClassify(10)
          train <- datastream_dataframe(data = dataTrain[-fold, ])
          totalInstances <- nrow(train$data)
          while (totalInstances > (train$state)) {
            it <- it + 1
            batch <- getBatch(train, dataLength)
            batch$class <- droplevels(batch$class)
            cat("Foram processadas: ", train$processed, "/", totalInstances, "\n")
            rownames(batch) <- as.character(1:nrow(batch))
            batchIds <- holdout(batch$class, ratio, mode = "random", seed = seed)
            batchLabeled <- batchIds$tr
            rm(batchIds)
            data <- newBase(batch, batchLabeled)
            data$class <- droplevels(data$class)
            classDist <- ddply(data[batchLabeled, ], ~class, summarise,
                               samplesClass = length(class))
            if (it > 1) {
              ensemblePred <- predictEnsemble(ensemble, data[batchLabeled, ],
              length(levels(data$class[batchLabeled])))
              cmLabeled <- table(ensemblePred, data$class[batchLabeled])
              ensembleAcc <- getAcc(cmLabeled)
              if (ensembleAcc < 0.8) {
                typeClassifier <- shuffleClassify(1)
                learner <- baseClassifiers[[typeClassifier]]
                initialAcc <- supAcc(learner, data[batchLabeled, ])
                oracle <- flexConC(learner, funcType[typeClassifier], classDist,
                                  initialAcc, "1", data, batchLabeled,
                                  learner@func)
                oraclePred <- predictClass(oracle, batch)
                ensemble <- swapEnsemble(ensemble, dataTrain, oracle)
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
          cmTest <- confusionMatrix(model, dataTest)
          if (length(rownames(cmTest)) != length(colnames(cmTest))) {
            cmTest <- fixCM(cmTest)
          }
          cat("\n\tCM TEST:\n")
          print(cmTest)
          accTest <- c(accTest, getAcc(cmTest))
          fmeasureTest <- c(fmeasureTest, fmeasure(cmTest))
          precisionTest <- c(precisionTest, precision(cmTest))
          recallTest <- c(recallTest, recall(cmTest))
        }
        end <- Sys.time()
        fileName <- paste(ratio * 100, "%DyDaSL", dataLength, ".txt", sep = "")
        writeArchive(paste("test", fileName, sep = ""), "../results/", dataName,
                     "DyDaSL", accTest, fmeasureTest, precisionTest, recallTest,
                     begin, end, epoch)
      }
    }
  }
}
