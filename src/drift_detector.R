#' @description Concept drift detection following the formula from
#'  'Knowledge Discovery from Data Streams' by João Gamma (p. 76)
#'  
#' @describeIn This code is a R port from the code avalaible in:
#'   https://github.com/blablahaha/concept-drift
#'  
#' @param delta default value is 0.005
#' @param lambda default value is 50
#' @param alpha default value is 1 - 0.0001 = 0.9999
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
                          initialize = function(delta=0.005, lambda=50,
                                               alpha=0.9999) {
                            stopifnot(is.numeric(delta), length(delta) == 1)
                            stopifnot(is.numeric(lambda), length(lambda) == 1)
                            stopifnot(is.numeric(alpha), length(alpha) == 1)
                            private$.delta <- delta
                            private$.lambda <- lambda
                            private$.alpha <- alpha
                          },
                          detect_drift = function(x) {
                            private$.count <- private$.count + 1
                            private$.x_mean <- (x + private$.x_mean
                                                  * (private$.count - 1))
                                               / private$.count
                            private$.sum <- private$.sum * private$.alpha
                                            + (x - private$.x_mean
                                               - private$.delta)
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


CPSSDS <- R6Class('CPSSDS',
                  private = list(
                    .last_chunk = NA,
                    .actual_chunk = NA,
                    .statistical_test = 'kolmogorov'
                  ),
                  public = list(
                    initialize = function(statistical_test='kolmogorov') {
                      stopifnot(is.character(statistical_test), 
                                str_to_lower(statistical_test) %in%
                                  c('kolmogorov', 't-test'),
                                length(statistical_test) == 1)
                      private$.statistical_test <- str_to_lower(statistical_test)
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
                    },
                    detect_drift = function() {
                      p <- self$eval_test()
                      if (p$p.value < 0.05) {
                        return(TRUE)
                      }
                      return(FALSE)
                    },
                    get_chunk = function(labels) {
                      private$.last_chunk <- c(private$.actual_chunk)
                      private$.actual_chunk <- labels
                    }
                  )
)


# base <- iris[sample(1:nrow(iris)),]

# all_levels <- sort(levels(base$Species))
# cpssds <- CPSSDS$new()
# train <- datastream_dataframe(data = base)
# data <- getBatch(train, 50)
# cpssds$get_chunk(data$Species)
# data <- getBatch(train, 50)
# data <- getBatch(train, 50)
# cpssds$get_chunk(data$Species)
# cat(cpssds$detect_drift())
