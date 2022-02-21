#' @description Concept drift detection Base class to provide some functions to
#'   manipulate the chunks. Abstract Class, not be initialized.
#'
BaseChunkDetector <- R6Class('BaseChunkDetector',
  private = list(
    .last_chunk = NA,
    .actual_chunk = NA
  ),
  public = list(
    get_chunk = function(labels) {
      private$.last_chunk <- c(private$.actual_chunk)
      private$.actual_chunk <- labels
    },
    detect_drift = function() {
      stop('This method must be implemented in inherit classes')
    }
  )
)
