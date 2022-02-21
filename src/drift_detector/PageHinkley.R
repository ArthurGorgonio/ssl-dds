#' @description Concept drift detection following the formula from
#'  'Knowledge Discovery from Data Streams' by João Gamma (p. 76)
#'  
#' @describeIn This code is a R port from the code available in:
#'   https://github.com/blablahaha/concept-drift
#'  
#' @param delta default value is 0.005
#' @param lambda default value is 50
#' @param alpha default value is 1 - 0.0001 = 0.9999
#'
#' @return TRUE if a drift was detected, FALSE otherwise.
#'
PageHinkley <- R6Class('PageHinkley',
  private = list(
    .delta = 0,
    .lambda = 0,
    .alpha = 0,
    .sum = 0,
    .x_mean = 0,
    .count = 0
  ),
  public = list(
    initialize = function(delta=0.005, lambda=50, alpha=0.9999) {
      stopifnot(is.numeric(delta), length(delta) == 1)
      stopifnot(is.numeric(lambda), length(lambda) == 1)
      stopifnot(is.numeric(alpha), length(alpha) == 1)
      private$.delta <- delta
      private$.lambda <- lambda
      private$.alpha <- alpha
    },
    detect_drift = function(x) {
      private$.count <- private$.count + 1
      private$.x_mean <- ((x + private$.x_mean * (private$.count - 1))
                          / private$.count)
      private$.sum <- (private$.sum * private$.alpha
                       + (x - private$.x_mean - private$.delta))
      if (private$.sum > private$.lambda) {
        self$reset_params()
        return(TRUE)
      }
      return(FALSE)
    },
    reset_params = function() {
      private$.sum <- 0
      private$.x_mean <- 0
      private$.count <- 0
    }
  )
)