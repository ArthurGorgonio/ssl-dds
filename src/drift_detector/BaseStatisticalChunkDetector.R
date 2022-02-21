#' @description Concept drift detection Base class to provide some functions to
#'   manipulate the chunks. Abstract Class, not be initialized.
#'
BaseStatisticalChunkDetector <- R6Class('BaseStatisticalChunkDetector',
  inherit = BaseChunkDetector,
  private = list(
    .statistical_test = NULL
  ),
  public = list(
    initialize = function(statistical_test) {
      private$.statistical_test <- statistical_test
    },
    eval_test = function() {
      stop('This method must be implemented in inherit classes')
    },
    detect_drift = function() {
      p <- self$eval_test()
      if (p$p.value < 0.05) {
        return(TRUE)
      }
      return(FALSE)
    }
  )
)
