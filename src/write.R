#' @description Write in the output file the content.
#'
#' @usage write_archive (title, prefix, content, dataName, modelName, acc, f1,
#'   preci, recall, begin, end, samplesPerIt, append = T, row = F, col = F,
#'   sep = " ")
#'
#' @param title The title of the file.
#' @param prefix The directory where the file be storage.
#' @param dataName The name of the current database.
#' @param modelName The name of the model/method that be used to classify the
#'   stream.
#' @param append The method to write in the archive.
#' @param acc A vector with all folds accuracy.
#' @param f1 A vector with all folds F-measure.
#' @param preci A vector with all folds precision measure.
#' @param recall A vector with all folds recall.
#' @param begin Time when start the database processing.
#' @param end Time when end the database processing.
#' @param samplesPerIt A vector with the number of samples to be added in each 
#'   iteration of the method.
#' @param append An optional parameter to append the current content in file.
#'   (Default TRUE).
#' @param row An optional parameter to write rows in the file. (Default FALSE).
#' @param col An optional parameter to write cols in the file. (Default FALSE).
#' @param sep An optional parameter to use in paste. (Default " " Single space).
#'
writeArchive <- function(title, prefix, dataName, modelName, acc, f1, preci,
                         recall, begin, end, it, append = T, row = F, col = F,
                         sep = " ") {
  acc <- round(acc, 4)
  f1 <- round(f1, 4)
  preci <- round(preci, 4)
  recall <- round(recall, 4)
  pattern <- "%d/%m/%Y %X"
  filePath <- paste(prefix, title, sep = "/")
  separ <- paste(rep("-", 80), collapse = "")
  metrics <- "\taccura\terror\tfmeasu\tprecis\trecall"
  dbName <- paste("@DATASET:", dataName)
  modelName <- paste("@Model:", modelName, sep = "\t")
  folds <- "@Folds\t: 10"
  headers <- paste(separ, it, separ, dbName, folds, modelName, separ, metrics,
                   separ, sep = "\n")
  line <- c()
  for (i in 1:10) {
    line <- paste(line,
                  paste("fold", i, ":\t", round(acc[i], 4), " ",
                        round(1 - acc[i], 4), " ", round(f1[i], 4), " ",
                        round(preci[i], 4), " ", round(recall[i], 4), sep = ""),
                  sep = "\n")
  }
  allMeans <- paste("AVERAG\t", round(mean(acc), 4), " ",
                    round(mean(round(1 - acc, 4)), 4), " ",
                    round(mean(f1), 4), " ", round(mean(preci), 4), " ",
                    round(mean(recall), 4), sep = "")
  line <- paste(line, separ, allMeans, separ, sep = "\n")
  time <- paste("BEGIN:\t", format(begin, pattern), "\nEND:\t",
                format(end, pattern), "\n\nTIME ELAPSED: ",
                round(end - begin, 4), " seconds\n", separ, sep = "")
  content <- paste(headers, line, time, sep = "\n")
  write(content, filePath, append = append, sep = sep)
}

headerDetailedOutputEnsemble <- function(title, prefix, dataName, modelName,
                                         append = T, row = F, col = F, 
                                         sep = " ") {
  pattern <- "%d/%m/%Y %X"
  details <- paste("In each iteration, info means:",
  "info01: ensemble size;",
  "info02: ensemble hits;",
  "info03: ensemble errors;",
  "info04: ensemble Acc;",
  "info05: ensemble F-Measure;",
  "info06: ensemble weight hits;",
  "info07: ensemble weight errors;",
  "info08: ensemble Acc Weighted;",
  "info09: ensemble F-Measure Weighted;",
  "info10: ensemble detect a drift;",
  "info11: Total processed instances;",
  "info12: Elapsed iteration time.",
  sep = "\n")
  filePath <- paste(prefix, title, sep = "/")
  separ <- paste(rep("-", 80), collapse = "")
  metrics <- format(c("info01", "info02", "info03", "info04", "info05", "info06",
                      "info07", "info08", "info09", "info10", "info11", "info12"),
                   justify = 'r', width = 11)
  dbName <- paste("@DATASET:", dataName)
  modelName <- paste("@Model:", modelName, sep = "\t")
  headers <- paste(separ, dbName, modelName, separ, details,
                   separ, sep = "\n")
  write(headers, filePath, append = append, sep = sep)
  cat(metrics, "\n", file = filePath, append = append)
}

detailedOutputEnsemble <- function(title, prefix, size, hits, error, acc, fscore,
                                   hits_weight, error_weight, acc_weight,
                                   fscore_weight, detect_drift, processed,
                                   enlapsed_it, append = T, row = F, col = F,
                                   sep = " ") {
  pattern <- "%d/%m/%Y %X"
  filePath <- paste(prefix, title, sep = "/")
  content <- format(c(size, hits, error, round(acc, 3), round(fscore, 3),
                      hits_weight, error_weight, round(acc_weight, 3),
                      round(fscore_weight, 3), detect_drift, processed,
                      paste(round(enlapsed_it, 3), "min", sep = " ")), 
                    justify = "right", width = 11, scientific = F)
  cat(content, "\n", file = filePath, append = append)
}
