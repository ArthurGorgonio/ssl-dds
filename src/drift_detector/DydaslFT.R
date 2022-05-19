#' @description Concept drift detection DyDaSL Fixed Threshold, it define a
#'   static threshold and evaluate the ensemble if the ensemble effectiveness
#'   metric maintain the minimum performance in the classification task during
#'   the next chunks.
#'
DydaslFT <- R6Class('DydaslFT',
  inherit = BaseDydasl,
  public = list(
    initialize = function(threshold = 0.8) {
      private$.threshold <- threshold
    },
    detect_drift = function() {
      actual_result <- as.numeric(private$.actual_value)
      if (actual_result < private$.threshold) {
        return (TRUE)
      }
      return (FALSE)
    }
  ),
  active = list(
    actual_value = function(value) {
      if (missing(value)) {
        private$.actual_value
      } else {
        private$.actual_value <- value
      }
    },
    threshold = function(value) {
      if (missing(value)) {
        private$.threshold
      }
    }
  )
)
