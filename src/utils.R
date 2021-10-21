atribArgs <- function(arguments, databases) {
  params <- list()
  params$ratios <- c(0.05, 0.1)
  params$lengthBatch <- c(500, 5000)
  params$iniIndex <- 1
  params$finIndex <- length(databases)
  params$seeds <- c(1, 3, 7)
  arg <- 1
  while (arg < length(arguments)) {
    param <- as.numeric(arguments[arg + 1])
    switch (arguments[arg],
            '-s' = {
              params$iniIndex <- param
            },
            '-e' = {
              params$finIndex <- param
            },
            '-l' = {
              params$lengthBatch <- param
            },
            '-r' = {
              params$ratios <- param
            },
            '-d' = {
              params$seeds <- param
            },
            '-h' = {
              cat("-s to start data set [1-11]\n-e to end data set [1-11]",
                  "-l to length batch\n-r to ratio\n-d to seed!!!")
            }
    )
    arg <- arg + 2
  }
  return(params)
}


#' @description Install packages if it was not installed and load them.
#' Why these packages?
#' plyr -> apply functions in hole dataset (lapply, ddply)
#' PMCMRplus -> Statistical Tests in a easy way
#' RWeka -> Weka classifiers they have the same struct nd are easy to use
#' RMOA -> API for MOA software for classifiers 
#' rminer -> Holdout function 
installNeedPacks <- function() {
  # packages <- c(, "DMwR", "RWeka", , "PMCMRplus", "ssc",
                # "RMOA", "foreign")
  packages <- c("plyr", "PMCMRplus", "RWeka", "RMOA", "rminer", "RPushbullet",
                "jsonlite")
  if (!require("BiocManager")) {
    install.packages("BiocManager")
  }
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      install.packages(pack)
    }
    library(pack, character.only = TRUE, verbose = F)
  }
}

installNeedPacks()

token <- fromJSON("../token.txt")

pbSetup(token$key, defdev = 1)
