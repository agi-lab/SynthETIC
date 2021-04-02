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
#' @param rfun optional alternative random sampling function; see Details for
#' default.
#' @param paramfun parameters for the random sampling function, as a function of
#' \code{claim_size}; see Details.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}, e.g.
#' if going with the default sampling distribution, you can specify a
#' `claim_size_benchmark_1` (below which claims are assumed to be settled with 1
#' or 2 payments) and `claim_size_benchmark_2` (below which claims are assumed
#' to be settled with 2 or 3 payments).
#'
#' @details Returns a list structure such that the \eqn{i}th component of the
#' list gives the number of partial payments required to settle each of the
#' claims that occurred in period \eqn{i}. It is assumed that at least one
#' payment is required i.e. no claims are settled without any single cash
#' payment.
#'
#' Let \eqn{M} represent the number of partial payments associated with a
#' particular claim. The default \code{simulate_no_pmt_function} is set up
#' such that if \code{claim_size} \eqn{\le} `claim_size_benchmark_1`,
#' \deqn{Pr(M = 1) = Pr(M = 2) = 1/2;} if `claim_size_benchmark_1` <
#' \code{claim_size} \eqn{\le} `claim_size_benchmark_2`,
#' \deqn{Pr(M = 2) = 1/3, Pr(M = 3) = 2/3;}
#' if \code{claim_size} > `claim_size_benchmark_2`
#' then \eqn{M} is geometric with minimum 4 and mean
#' \deqn{min(8, 4 + log(claim_size/claim_size_benchmark_2)).}
#'
#' Alternative sampling distributions are supported through `rfun` (the random
#' generation function) and `paramfun` (which returns the parameters of `rfun`
#' as a function of `claim_size`). The `paramfun` should return the distribution
#' parameters in a **vector**, e.g. for gamma distribution `paramfun` should
#' return a value in the format of `c(shape = , scale = )`. If a `rfun` is
#' specified without a `paramfun`, `SynthETIC` will try to proceed without
#' parameterisation (i.e. directly calling `rfun` with `claim_size`), and if it
#' fails, then return an error message.
#'
#' @examples
#' n_vector <- claim_frequency(I = 10)
#' # with default simulation function
#' no_payments <- claim_payment_no(n_vector, claim_size(n_vector))
#' no_payments[[1]] # number of payments for claims incurred in period 1
#'
#' # modify the lower benchmark value
#' claim_payment_no(n_vector, claim_size(n_vector),
#'                  claim_size_benchmark_1 = 5000)
#' @export
claim_payment_no <- function(
  frequency_vector, claim_size_list, rfun, paramfun, ...) {

  if (!missing(rfun) & missing(paramfun)) {
    # we will see if we can continue without parameterisation
    # e.g. if the input rfun is directly a function of claim_size, and no
    # transformation is required
    paramfun <- function(...) {
      c(...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # default simulation function for the number of partial payments
  if (missing(rfun)) {
    rfun <- function(
      # n = number of observations
      n, claim_size, claim_size_benchmark_1, claim_size_benchmark_2) {

      if (n != length(claim_size)) {
        stop("The number of claims does not match.")
      }

      # default benchmark values
      if (missing(claim_size_benchmark_1)) {
        claim_size_benchmark_1 <- rep(0.0375 * .pkgenv$ref_claim, n)
      }
      if (missing(claim_size_benchmark_2)) {
        claim_size_benchmark_2 <- rep(0.075 * .pkgenv$ref_claim, n)
      }

      # generate the number of payments according to where the claim size sits
      # construct the region indicators
      test_1 <- (claim_size_benchmark_1 < claim_size & claim_size <= claim_size_benchmark_2)
      test_2 <- (claim_size > claim_size_benchmark_2)

      no_pmt <- vector(length = n)
      for (i in 1:n) {
        if (claim_size[i] <= claim_size_benchmark_1[i]) {
          no_pmt[i] <- sample(c(1, 2), size = 1, replace = T, prob = c(1/2, 1/2))
        } else if (claim_size[i] <= claim_size_benchmark_2[i]) {
          no_pmt[i] <- sample(c(2, 3), size = 1, replace = T, prob = c(1/3, 2/3))
        } else {
          no_pmt_mean <- pmin(8, 4 + log(claim_size[i]/claim_size_benchmark_2[i]))
          prob <- 1 / (no_pmt_mean - 3)
          no_pmt[i] <- stats::rgeom(n = 1, prob = prob[test_2]) + 4
        }
      }

      no_pmt
    }

    # the default rfun directly takes claim_size as an input, so the "empty"
    # paramfun would do the trick
    paramfun <- function(...) {
      c(...)
    }
  }

  I <- length(frequency_vector)
  no_claims <- sum(frequency_vector)
  params <- mapply(paramfun,
                   claim_size = unlist(claim_size_list, use.names = FALSE),
                   ...)

  # if params only has one parameter, asplit() won't work
  if (!is.null(names(params))) {
    params_split <- split(unname(params), names(params))
  } else {
    params_split <- asplit(params, 1)
  }

  # do.call rfun, but ignore unused arguments
  args <- as.list(params_split)
  keep_names <- c(intersect(names(args), names(formals(rfun))))
  keep_formals <- c(list(n = no_claims), args[keep_names])

  if (paramfun_filled) {
    # check if the "empty" paramfun is sufficient to call the rfun
    tt <- try(no_payments_vect <- do.call(rfun, keep_formals), TRUE)
    if (methods::is(tt, "try-error")) {
      stop("need to specify 'paramfun' for the sampling distribution")
    }
  } else {
    no_payments_vect <- do.call(rfun, keep_formals)
  }

  no_payments_list <- to_SynthETIC(no_payments_vect, frequency_vector)
  no_payments_list

}
