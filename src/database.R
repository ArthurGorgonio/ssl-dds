#' @description This function get a batch with n samples of a data stream.
#'
#' @param database the data frame (data stream).
#' @param n the number of the samples which be select to compose a batch.
#'
#' @return a batch with n samples and all features and class of the data set.
#'
getBatch <- function(database, n) {
  return(database$get_points(n))
}

#' @description Read a data set from a arff file
#'
#' @param datasetName The name of the data set which be read.
#' @param fold The fold location on the computer.
#'
#' @return The data set loaded in memory.
#'
getDatabase <- function(datasetName, fold = "../datasets") {
  database <- read.arff(paste(fold, datasetName, sep = "/"))
  return(database)
}


#' @description This function read a data stream and transform to spetial type
#'  of the dataframe.
#'
#' @param dbName the name of the data stream (data base).
#'
#' @return an object of the `datastream_dataframe` class.
#'
readData <- function(dbName, fold = "../datasets") {
  data <- datastream_dataframe(factorise(getDatabase(dbName)))
  return(data)
}
