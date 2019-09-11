#' @description Install packages if it was not installed and load them.
#'
installNeedPacks <- function() {
  packages <- c("ssc", "plyr", "DMwR", "DMwR2", "RWeka", "rminer", "e1071",
                "ggplot2", "PMCMR", "RMOA")
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      install.packages(pack)
    }
    library(pack, character.only = TRUE, verbose = F)
  }
}

installNeedPacks()
