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
page_hinkley <- setRefClass("page_hinkley",
                            fields = list(delta_='numeric', lambda_='numeric',
                                        alpha_='numeric', sum_='numeric',
                                        x_mean_='numeric', num_='numeric',
                                        change_detected='logical'),
                            methods = list(
                              initialize = function(..., delta=0.005, lambda=50,
                                                    alpha=0.9999) {
                                delta_ <<- delta
                                lambda_ <<- lambda
                                alpha_ <<- alpha
                                sum_ <<- 0
                                x_mean_ <<- 0
                                num_ <<- 0
                                change_detected <<- FALSE
                              },
                              .detect_drift = function(x) {
                                num_ <<- num_ + 1
                                x_mean_ <<- (x + x_mean_ * (num_ - 1)) / num_
                                sum_ <<- sum_ * alpha_ + (x - x_mean_ - delta_)
                                if (sum_ > lambda_) {
                                  change_detected <<- TRUE
                                  .reset_params()
                                } else {
                                  change_detected <<- FALSE
                                }
                                return (change_detected)
                              },
                              .reset_params = function() {
                                num_ <<- 0
                                x_mean_ <<- 0
                                sum_ <<- 0
                              },
                              set_input = function(x) {
                                return(.detect_drift(x))
                              }
                            )
)

CPSSDS <- setRefClass('CPSSDS',
                      fields = list(last_chunk='factor',
                                    actual_chunk='factor',
                                    statistical_test='character',
                                    change_detected='logical'),
                      methods = list(
                        initialize = function(...,
                                              statistical_test='kolmogorov') {
                          statistical_test <<- statistical_test
                          change_detected <<- FALSE
                        },
                        .eval_test = function() {
                          switch (statistical_test,
                            'kolmogorov' = {
                              test <- ks.test(as.numeric(last_chunk),
                                              as.numeric(actual_chunk))
                            },
                            't-test' = {
                              test <- t.test(as.numeric(last_chunk),
                                             as.numeric(actual_chunk))
                            }
                          )
                          return(test)
                        },
                        detect_drift = function() {
                          change_detected <<- FALSE
                          p <- .eval_test()
                          if (p$p.value < 0.05) {
                            change_detected <<- TRUE
                          }
                          return(change_detected)
                        },
                        get_chunk = function(labels) {
                          last_chunk <<- actual_chunk
                          actual_chunk <<- labels
                        }
                      )
)
