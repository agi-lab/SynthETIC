###############################################################################
##                           1. Claim occurrence                             ##
###############################################################################

#' Claim Frequency
#'
#' Returns the number of insurance claims occurring in each of the periods.
#'
#' @param I number of claims development periods considered.
#' @param E **effective annual** exposure associated with each period (vector).
#' @param freq expected frequency per unit exposure for each period (vector).
#' @param cdf optional cumulative distribution function to be sampled from.
#' @param range support of the custom \code{cdf}, with default (-1e200, 1e200).
#' @param ... other arguments/parameters to be passed onto \code{cdf}.
#' @details Unless otherwise specified, \code{claim_frequency()} assumes the
#' claim frequency follows a Poisson distribution with mean equal to the
#' product of exposure \code{E} associated with period \eqn{i} and expected
#' claim frequency \code{freq} per unit exposure for that period. \cr \cr
#' If no arguments are provided, by default \code{claim_frequency()} assumes
#' a total of 40 development periods, constant exposure rate at 12000 per year
#' and constant frequency at 0.03 per unit of exposure. \cr \cr
#' Pre-defined distribution functions such as \code{ppois} are supported.
#' @examples
#' no_period <- 40
#' exposure <- c(rep(12000, no_period))
#' exp_freq <- c(rep(0.03, no_period))
#' # returns the same result as claim_frequency()
#' claim_frequency(I = no_period, E = exposure, freq = exp_freq)
#'
#' # some custom pre-defined distribution function
#' claim_frequency(I = 10, cdf = ppois, range = c(0, 1000), lambda = 80)
#' @export
claim_frequency <- function(I = 40, E = 12000, freq = 0.03, cdf, range, ...) {
  if (!missing(cdf)) {
    # if a custom cumulative distribution function is provided
    if (missing(range)) {range <- c(-1e200, 1e200)}
    simulate_cdf(I, cdf, range, ...)
  } else {
    # if no such function is provided, we assume a Poisson distribution
    stats::rpois(I, E * .pkgenv$time_unit * freq)
  }
}

#' Claim Occurrence Times
#'
#' Returns the occurrence times of each of the claims occurring in each of
#' the periods, assuming the occurrence time of any claim in period \eqn{i}
#' is uniformly distributed between times \eqn{i - 1} and \eqn{i}.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @return A list of occurrence times such that the \eqn{i}th component of the
#' list gives the claim occurrence time for all claims that occurred in period
#' \eqn{i}.
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#' # occurrence time for all claims originating from period 1
#' claim_occurrence(n_vector)[[1]]
#' @export
claim_occurrence <- function(frequency_vector) {
  I <- length(frequency_vector)
  occurrence_times <- vector("list", I)
  for (i in 1:I) {
    occurrence_times[[i]] <- stats::runif(frequency_vector[i], min = i - 1, max = i)
  }
  occurrence_times
}
