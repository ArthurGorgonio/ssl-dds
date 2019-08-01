#' @description Install packages if it was not installed and load them.
installNeedPacks <- function() {
  packages <- c("ssc", "plyr", "DMwR", "DMwR2", "RWeka", "rminer", "e1071",
                "ggplot2", "stats", "PMCMR", "PMCMRplus")
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      install.packages(pack)
    }
    library(pack, character.only = TRUE)
  }
}

#' @description This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/Projects/Mestrado")
  } else {
    setwd("C:\\Projects\\Mestrado")
  }
}
