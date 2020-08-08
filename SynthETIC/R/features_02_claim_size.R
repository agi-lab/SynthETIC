###############################################################################
##                   2. Claim size (constant dollar values)                  ##
###############################################################################

#' Claim Size
#'
#' Simulates and returns the size of each of the claims occurring in each of
#' the periods, given its cumulative distribution function.
#' \cr \cr Note that \code{claim_size()} aims to model the claim sizes
#' **without inflation**.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param S_cdf (optional) cumulative distribution function to be sampled from.
#' @param range the possible set of values of claim sizes, with default
#' (0, 1e24).
#' @param ... other arguments/parameters to be passed onto \code{S_cdf}.
#' @return A list of claim sizes such that the \eqn{i}th component of the
#' list gives the sizes for all claims that occurred in period \eqn{i}.
#' @details By default \code{claim_size()} assumes a left truncated power
#' normal distribution: \eqn{S^0.2 ~ Normal (9.5, sd = 3)}, left truncated
#' at 30. The truncation is done via resampling for rejected values. \cr \cr
#' Users can opt to use predefined distributions if desired. See Examples.
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#' claim_size(n_vector)[[1]] # gives the sizes for all
#'                           # all claims incurred in period 1
#'
#' # with some custom pre-defined distribution function
#' claim_size(n_vector, stats::pweibull, shape = 4, scale = 100000)[[1]]
#' @export
claim_size <- function(frequency_vector, S_cdf, range = c(0, 1e24), ...) {
  I <- length(frequency_vector)
  total_claim <- sum(frequency_vector)

  if (missing(S_cdf)) {

    # if no distribution function is provided, use default power normal
    s <- (stats::rnorm(total_claim, mean = 9.5, sd = 3))^5
    while (any(s < 30)) {
      for (j in which(s < 30)) {
        s[j] <- (stats::rnorm(1, mean = 9.5, sd = 3))^5
      }
    }
    # re-scale the claim sizes according to ref_claim value provided
    s <- .pkgenv$ref_claim / 200000 * s
    claim_sizes <- split(s, rep(1:I, frequency_vector))

  } else {
    claim_sizes <- vector("list", I)
    for (i in 1:I) {
      claim_sizes[[i]] <- simulate_cdf(frequency_vector[i], S_cdf, range, ...)
      # re-scale the claim sizes according to ref_claim value provided
      claim_sizes[[i]] <- .pkgenv$ref_claim / 200000 * claim_sizes[[i]]
    }
  }

  claim_sizes
}
