#' @description Install packages if it was not installed and load them.
#'
installNeedPacks <- function() {
  packages <- c("plyr", "DMwR", "DMwR2", "RWeka", "rminer", "e1071", "ggplot2",
                "PMCMR", "RMOA", "ssc", "scmamp")
  if (!require("BiocManager")) {
    install.packages("BiocManager")
  }
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      BiocManager::install(pack)
    }
    library(pack, character.only = TRUE, verbose = F)
  }
}

installNeedPacks()
