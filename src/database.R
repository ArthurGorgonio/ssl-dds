#' @description This function get a batch with n samples of a data stream.
#'
#' @param database the data frame (data stream).
#' @param n the number of the samples which be select to compose a batch.
#'
#' @return a batch with n samples and all features and class of the data set.
#'
getBatch <- function(database, n) {
  len <- abs(nrow(database$data) - (database$state + n))
  return(database$get_points(min(n, len)))
}

#' @description Read a data set from a arff file
#'
#' @param datasetName The name of the data set which be read.
#' @param path The location on the computer.
#'
#' @return The data set loaded in memory.
#'
getDatabase <- function(datasetName, path = "../datasets") {
  database <- read.arff(paste(path, datasetName, sep = "/"))
  return(database)
}


#' @description This function read a data stream and transform to spetial type
#'  of the dataframe.
#'
#' @param dbName The name of the data stream (data base).
#' @param path The location on the computer.
#'
#' @return An object of the `datastream_dataframe` class.
#'
readData <- function(dbName, path = "../datasets") {
  data <- datastream_dataframe(factorise(getDatabase(dbName, path)))
  return(data)
}
