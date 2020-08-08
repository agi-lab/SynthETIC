###############################################################################
##                       5. Number of Partial payments                       ##
###############################################################################

#' Number of Partial Payments
#'
#' Simulates and returns the number of partial payments required to settle each
#' of the claims occurring in each of the periods.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param claim_size_list list of claim sizes.
#' @param simulate_no_pmt_function a function that generates the number of
#' partial payments associated with a particular claim, conditional on
#' \code{claim_size} (see Details for the default simulation function).
#' @param claim_size_benchmark_1 a value below which claims are assumed to be
#' settled with 1 or 2 payments, unless an alternative
#' `simulate_no_pmt_function` is specified (default 0.0375 * `ref_claim`).
#' @param claim_size_benchmark_2 a second criterion below which claims are
#' assumed to be settled with 2 or 3 payments, unless an alternative
#' `simulate_no_pmt_function` is specified (default 0.075 * `ref_claim`).
#' @details Returns a list structure such that the \eqn{i}th component of the
#' list gives the number of partial payments required to settle each of the
#' claims that occurred in period \eqn{i}. It is assumed that at least one
#' payment is required i.e. no claims are settled without any single cash
#' payment. \cr \cr
#' Let \eqn{M} represent the number of partial payments associated with a
#' particular claim. The default \code{simulate_no_pmt_function} is set up
#' such that if \code{claim_size} \eqn{\le} `claim_size_benchmark_1`,
#' \deqn{Pr(M = 1) = Pr(M = 2) = 1/2;} if `claim_size_benchmark_1` <
#' \code{claim_size} \eqn{\le} `claim_size_benchmark_2`,
#' \deqn{Pr(M = 2) = 1/3, Pr(M = 3) = 2/3;}
#' if \code{claim_size} > `claim_size_benchmark_2`
#' then \eqn{M} is geometric with minimum 4 and mean
#' \deqn{min(8, 4 + log(claim_size/claim_size_benchmark_2)).}
#' @examples
#' n_vector <- claim_frequency(I = 10)
#' # with default simulate_no_pmt_function
#' no_payments <- claim_payment_no(n_vector, claim_size(n_vector))
#' no_payments[[1]] # number of payments for claims incurred in period 1
#' @export
claim_payment_no <- function(
  frequency_vector,
  claim_size_list,
  simulate_no_pmt_function,
  claim_size_benchmark_1 = 0.0375 * .pkgenv$ref_claim,
  claim_size_benchmark_2 = 0.075 * .pkgenv$ref_claim) {

  # default simulate_no_pmt_function
  if (missing(simulate_no_pmt_function)) {
    simulate_no_pmt_function <- function(claim_size, claim_size_benchmark_1, claim_size_benchmark_2) {
      if (claim_size <= claim_size_benchmark_1) {
        return(sample(c(1, 2), size = 1, replace = TRUE, prob = c(1/2, 1/2)))
      } else if (claim_size_benchmark_1 < claim_size & claim_size <= claim_size_benchmark_2) {
        return(sample(c(2, 3), size = 1, replace = TRUE, prob = c(1/3, 2/3)))
      } else {
        no_pmt_mean <- min(8, 4 + log(claim_size/claim_size_benchmark_2))
        prob <- 1/(no_pmt_mean - 3)
        return(stats::rgeom(1, prob) + 4)
      }
    }
  }
  # vectorise simulate_no_pmt_function so it applies to vector arguments
  simulate_no_pmt_function <- Vectorize(simulate_no_pmt_function)

  I <- length(frequency_vector)
  no_payments <- vector("list", I)
  for (i in 1:I) {
    no_payments[[i]] <- simulate_no_pmt_function(claim_size_list[[i]],
                                                 claim_size_benchmark_1, claim_size_benchmark_2)
  }
  no_payments
}
