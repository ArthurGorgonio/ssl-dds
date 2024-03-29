#' @description This function counts the number of the samples per class in a
#' vector
#'
#' @usage distSamples (labels)
#'
#' @param labels The class column in the dataset
#'
#' @return A vector with the number of the samples of the each class
#'
distSamples <- function(labels) {
  x <- c()
  for (class in levels(labels)) {
    x <- c(x, length(which(labels == class)))
  }
  return(x)
}

#' @description This function counts the number of the samples per class in a
#'  vector
#'
#' @usage samplesPerClass (labels, k = 10)
#'
#' @param labels The class column in the dataset
#' @param k The number of the folds to split the data
#'
#' @return A vector with the number of the samples of each class than each fold
#'
samplesPerClass <- function(labels, k) {
  vector <- c()
  nSamplesClass <- distSamples(labels)
  for (i in 1:length(levels(labels))) {
    vector[i] <- nSamplesClass[i] / k
  }
  return(vector)
}

#' @description This function returns a list with k folds
#'
#' @usage stratifiedKFold(database, labels, k = 10)
#'
#' @param database The database are you using without class column
#' @param labels The class column in the dataset
#' @param k The number of the folds to split the data
#'
#' @return A list with k sublists contains the samples
#'
#' @examples
#' data <- iris[, 1:(length(iris)-1)]
#' class <- iris$Species
#' data <- cbind(data, class = class)
#' folds <- stratifiedKFold(data, class, k = 10)
#'
stratifiedKFold <- function(database, labels, k = 10) {
  if (k > 1) {
    folds <- c()
    namesId <- row.names(database)
    eachClass <- samplesPerClass(labels, k)
    for (i in 1:k) {
      selectedSamples <- c()
      for (j in 1:length(levels(labels))) {
        range <- which(database[namesId, 'class'] == levels(database$class)[j])
        selectedSamples <- c(selectedSamples, sample(range, eachClass[j]))
      }
      folds <- c(folds, list(as.integer(namesId[selectedSamples])))
      namesId <- namesId[-selectedSamples]
    }
    if (length(namesId) != 0) {
      remainder <- c()
      for (lvls in levels(database$class)) {
        remainder <- c(remainder, list(sample(1:k, length(
          which(database$class[as.integer(namesId)] == lvls)), replace = T)))
      }
      names(remainder) <- levels(database$class)
      for (label in names(remainder)) {
        if (length(remainder[[label]]) != 0) {
          for (k in remainder[[label]]) {
            content <- sample(namesId[which(database[as.integer(namesId),
                                                     'class'] == label)], 1,
                              replace = T)
            folds[[k]] <- c(folds[[k]], as.integer(content))
            namesId <- namesId[-which(namesId == content)]
          }
        }
      }
    }
    return(folds)
  } else {
    stop('K need to be higger than 1')
  }
}
