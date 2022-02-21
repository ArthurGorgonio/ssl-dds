#' @description Concept drift detection following the CPSSDS approach
#'  
#' @describeIn This code is a R port from the code available in:
#'   https://github.com/neginsmd/CPSSDS
#'  
#' @param statistical_test which test should be applied over both chunks
#'   `default = 'kolmogorov'`
#' 
#' @return TRUE if a drift was detected, FALSE otherwise.
#'
CPSSDS <- R6Class('CPSSDS',
  inherit = BaseStatisticalChunkDetector,
  public = list(
    initialize = function(statistical_test='kolmogorov') {
      stopifnot(is.character(statistical_test), 
                str_to_lower(statistical_test) %in% c('kolmogorov', 't-test'),
                length(statistical_test) == 1)
      super$initialize(str_to_lower(statistical_test))
    },
    eval_test = function() {
      switch (private$.statistical_test,
        'kolmogorov' = {
          test <- ks.test(as.numeric(private$.last_chunk),
                          as.numeric(private$.actual_chunk))
        },
        't-test' = {
          test <- t.test(as.numeric(private$.last_chunk),
                         as.numeric(private$.actual_chunk))
        }
      )
      return(test)
    }
  )
)
