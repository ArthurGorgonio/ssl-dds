#' @description This function check the actual directory has a sub directory
#'   called src if exists it's a new working directory
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
#' , 9, 12, 18, 29, 32, 36, 44, 49, 73, 80, 92, 100, 154, 201, 273, 310, 374,
#' 435, 559, 623, 828, 945, 3341, 3431, 3581, 4134, 8999
options(java.parameters = "-Xmx4g")
#'

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
meansFlexConC1S <- c()
meansFlexConC1V <- c()
databases <- list.files(path = "../datasets")
lengthBatch <- 5000
for (dataLength in lengthBatch) {
  defines()
  for (dataset in databases) {
    dataName <- strsplit(dataset, ".", T)[[1]][1]
    cat(dataName)
    epoch <- 0
    for (seed in seeds) {
      epoch <- epoch + 1
      cat("\n\n\nRODADA: ", epoch, "\n\n\n\n")
      set.seed(seed)
      originalDB <- getDatabase(dataset)
      classifiers <- baseClassifiers
      dataL <- holdout(originalDB$class, .75, mode = "random", seed = seed)
      dataTrain <- originalDB[dataL$tr, ]
      folds <- stratifiedKFold(dataTrain, dataTrain$class)
      dataTest <- originalDB[dataL$ts, ]
      cl <- match(label, colnames(dataTest))
      for (learner in classifiers) {
        cat("\n\nClassifier:\t", learner$type, "\n\n\n\n")
        begin <- Sys.time()
        accTest <- c()
        fmeasureTest <- c()
        precisionTest <- c()
        recallTest <- c()
        for (fold in folds) {
          train <- datastream_dataframe(data = dataTrain[-fold, ])
          model <- trainMOA(model = learner, formula = form, data = train,
                            chunksize = dataLength)
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
        fileName <- paste(toupper(learner$type), dataLength, ".txt", sep = "")
        writeArchive(paste("test", fileName, sep = ""), "../results/", dataName,
                     model$model$type, accTest, fmeasureTest, precisionTest,
                     recallTest, begin, end, epoch)
      }
    }
  }
}
