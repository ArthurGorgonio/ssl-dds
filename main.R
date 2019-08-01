#' @description This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/Projects/Mestrado")
  } else {
    setwd("C:\\Projects\\Mestrado")
  }
}

args = commandArgs(trailingOnly=TRUE)
if ((args == "-h") || (args == "--help")) {
  cat("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
       "\n3 - JRip\n4 - IBk")
} else if ((as.integer(args) == F) || (is.na(as.integer(args))) ||
           (as.integer(args) > 4) || (as.integer(args) < 1)) {
  stop("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
       "\n3 - JRip\n4 - IBk")
} else {
  args <- 1
  setWorkspace()
  source("functions.R")
  source("utils.R")
