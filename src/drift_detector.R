setClass("page_hinkley", representation(delta_='numeric', lambda_='numeric',
                                        alpha_='numeric', sum_='numeric',
                                        x_mean_='numeric', num_='numeric',
                                        change_detected='logical'))

#' --------------------------------------------------------------
#' constructor function
page_hinkley_detector <- function(delta=0.005, lambda=50, alpha=0.9999) {
  # if (missing(delta)) stop("\nYou need to provide delta parameter.\n")
  # if (missing(lambda)) stop("\nYou need to provide lambda parameter.\n")
  # if (missing(alpha)) stop("\nYou need to provide alpha parameter.\n")
  new("page_hinkley", delta_=delta, lambda_=lambda, alpha_=alpha, sum_=0,
      x_mean_=0, num_=0, change_detected=FALSE)
}

#' --------------------------------------------------------------
# Generic Methods
# setGeneric("detect_drift", function(self, x) standardGeneric("detect_drift"))
# setGeneric('reset_params', function(self) standardGeneric('reset_params'))
# setGeneric('set_input', function(self, x) standardGeneric('set_input'))

#' --------------------------------------------------------------
# Methods
page_hinkley._detect_drift <- function(self, x) {
  self@num_ <- self@num_ + 1
  self@x_mean_ <- (x + self@x_mean_ * (self@num_ - 1)) / self@num_
  self@sum_ <- self@sum_ * self@alpha_ + (x - self@x_mean_ - self@delta_)
  
  if (self@sum_ > self@lambda_) {
    self@change_detected = TRUE
    self <- page_hinkley._reset_params(self)
  } else {
    self@change_detected = FALSE
  }
  return (self)
}

page_hinkley._reset_params <- function(self) {
  self@num_ = 0
  self@x_mean_ = 0
  self@sum_ = 0
  return (self)
}

page_hinkley.set_input <- function(self, x) {
  self <- page_hinkley._detect_drift(self, x)
  return(self)
}