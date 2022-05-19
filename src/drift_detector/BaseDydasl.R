#' @description Concept drift detection DyDaSL Base class to provide some
#'   functions to manipulate the chunks. Abstract Class, not be initialized.
#'
BaseDydasl <- R6Class('BaseDydasl',
  private = list(
    .threshold = NA,
    .actual_value = NA
  ),
  public = list(
    initialize = function(threshold) {
      private$.threshold <- threshold
    }
  )
)
