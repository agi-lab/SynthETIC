#############################################################################
##                     Statistical Helper Functions                        ##
#############################################################################

#' Inverse Tranform Sampling
#'
#' Generates sample numbers at random from any probability distribution
#' given its cumulative distribution function. Pre-defined distribution
#' functions such as \code{pnorm} are supported. \cr \cr
#' See \href{https://en.wikipedia.org/wiki/Inverse_transform_sampling}{here}
#' for the algorithm.
#'
#' @param n number of observations.
#' @param cdf cumulative distribution function to be sampled from.
#' @param range support of the given \code{cdf}.
#' @param ... other arguments/parameters to be passed onto \code{cdf}.
#' @examples
#' simulate_cdf(10, pnorm)
#' simulate_cdf(10, pbeta, shape1 = 2, shape2 = 2)
#' @export
simulate_cdf <- function(n, cdf, range = c(-1e200, 1e200), ...) {

  solve_cdf <- function(x, u) {
    cdf(x, ...) - u
  }

  # quantile_cdf returns the quantile for a distribution defined by cdf
  quantile_cdf <- function(u) {
    stats::uniroot(solve_cdf, interval = range, tol = 1e-16, u = u)$root
  }
  quantile_cdf <- Vectorize(quantile_cdf)

  prob <- stats::runif(n)
  quantile_cdf(prob)

}


#' Estimating Weibull Parameters
#'
#' Returns the Weibull shape and scale parameters given the mean and the CoV
#' of the target Weibull distribution.
#'
#' @param target_mean mean of the target Weibull distribution.
#' @param target_cv CoV of the target Weibull distribution.
#' @examples
#' get_Weibull_parameters(target_mean = 100000, target_cv = 0.60)
#' get_Weibull_parameters(target_mean = c(100000, 200000, 300000),
#'                        target_cv = 0.60)
#' @export
get_Weibull_parameters <- function(target_mean, target_cv) {

  # f: a helper function used to determine the Weibull shape and scale parameters from mean and CV
  # by computing the difference between the CV implied by a guess shape parameter and the target CV
  f <- function(a, cv) {
    (sqrt(gamma(1 + 2/a) - (gamma(1 + 1/a))^2)/gamma(1 + 1/a)) - cv
  }

  Weibull_shape <- stats::uniroot(f, cv = target_cv, lower = 0.1, upper = 100, tol = 1e-16)$root
  Weibull_scale <- target_mean / gamma(1 + 1/Weibull_shape)

  c(Weibull_shape, Weibull_scale)
}

get_Weibull_parameters <- Vectorize(get_Weibull_parameters)


#' Estimating Beta Parameters
#'
#' Returns the Beta parameters given the mean and the CoV of the target Beta
#' distribution.
#'
#' @param target_mean mean of the target Beta distribution (between 0 and 1).
#' @param target_cv CoV of the target Beta distribution.
#' @examples
#' get_Beta_parameters(target_mean = 0.5, target_cv = 0.20)
#' get_Beta_parameters(target_mean = 0.5,
#'                     target_cv = c(0.10, 0.20, 0.30))
#' @export
get_Beta_parameters <- function(target_mean, target_cv) {
  target_var <- (target_cv * target_mean)^2
  alpha <- ((1 - target_mean)/target_var - 1/target_mean) * target_mean^2
  beta <- alpha * (1/target_mean - 1)
  return(c(alpha, beta))
}
get_Beta_parameters <- Vectorize(get_Beta_parameters)


#' Coefficient of Variation
#'
#' Returns the observed coefficient of variation (CoV) of a given sample
#' \code{x}. \cr \cr
#' If \code{na.rm} is true then missing values are removed before
#' computation proceeds, as in the case of the \code{mean()} function.
#'
#' @param x a numeric vector.
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @details The coefficient of variation is defined as is defined as the
#' ratio of the standard deviation to the mean. It shows the extent of
#' variability in relation to the mean of the population. \cr \cr
#' \code{cv()} estimates the CoV of a given sample by computing the ratio of
#' the sample standard deviation (see \code{stats::sd}) to the sample mean.
#' @examples
#' cv(1:10)
#' @export
cv <- function(x, na.rm = TRUE) {
  stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
