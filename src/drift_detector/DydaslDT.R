#' @description Concept drift detection DyDaSL Dynamic Threshold, it uses the
#'   ensemble effectiveness metric to compute the next threshold and use this
#'   new value to perform the drift detection.
#'
DydaslDT <- R6Class('DydaslDT',
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
      if (value >= 0.0 && value <= 1.0) {
        private$.threshold <- value
      } else {
        stop('can not set threshold ', value, ' because it is',
             ' greater than 1.0 and below of 0', call.=FALSE)
      }
    }
  )
)
