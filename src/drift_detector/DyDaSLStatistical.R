#' @description Concept drift detection following two main condition, ensemble
#'   effectiveness and statistical test
#'  
#' @param statistical_test which test should be applied over both chunks
#'   `default = 'wilcoxon'`
#' 
#' @return TRUE if a drift was detected, FALSE otherwise.
#'
DyDaSLWS <- R6Class('DyDaSLWS',
  inherit = BaseStatisticalChunkDetector,
  public = list(
    initialize = function(statistical_test = 'wilcoxon') {
      stopifnot(is.character(statistical_test), 
                str_to_lower(statistical_test) %in% c('wilcoxon', 't-test'),
                length(statistical_test) == 1)
      super$initialize(str_to_lower(statistical_test))
    },
    eval_test = function() {
      switch (private$.statistical_test,
        'wilcoxon' = {
          test <- wilcox.test(as.numeric(private$.last_chunk),
                              as.numeric(private$.actual_chunk))
        },
        't-test' = {
          test <- t.test(as.numeric(private$.last_chunk),
                         as.numeric(private$.actual_chunk))
        }
      )
      return (test)
    },
    detect_drift = function() {
      if (mean(private$.actual_chunk) < mean(private$.last_chunk)) {
        return (super$detect_drift())
      } else {
        return (FALSE)
      }
    }
  )
)
