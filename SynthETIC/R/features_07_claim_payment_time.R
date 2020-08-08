###############################################################################
##                   7. Distribution of payments over time                   ##
###############################################################################

#' Inter-Partial Delays
#'
#' Simulates and returns the inter-partial delays (i.e. the delay of one
#' partial payment relative to the previous) of each payment for each of the
#' claims occurring in each of the periods.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param claim_size_list list of claim sizes.
#' @param no_payments_list list of number of partial payments.
#' @param settlement_list list of settlement delays.
#' @param settlement_mean_function target mean of the settlement delay
#' as a function of \code{claim_size} and \code{occurrence_period} (see the
#' documentation of \code{claim_closure} for more details).
#' @param simulate_delay_function a function that generates the payment delay
#' pattern of a particular claim (as a vector of size = \code{no_pmt}), taking
#' as input \code{no_pmt}, \code{claim_size}, \code{setldel},
#' \code{occurrence_period}, and \code{setldel_mean_function} (see Details
#' for the default simulation algorithm).
#'
#' @details Returns a compound list structure such that the \eqn{j}th component
#' of the \eqn{i}th sub-list gives the payment delay pattern (as a vector) for
#' the \eqn{j}th claim of occurrence period \eqn{i}. \cr \cr
#' The default \code{simulate_delay_function} is split into 2 cases. \cr \cr
#' **Case 1: claims with at least 4 partial payments.** The simulation takes
#' 2 steps. \cr
#' First we sample the last payment delay from a Weibull distribution with
#' mean = 1 quarter (automatically converted to the relevant `time_unit`, a
#' global variable that the user is required to define at the top of their code)
#' and CoV = 20%. Then we sample the remaining payment delays
#' from a second Weibull distribution with CoV at 35% and
#' \deqn{mean = settlement_mean_function(claim_size, occurrence_period)/no_pmt}
#' where \code{settlement_mean_function()} is the function that
#' we used in \code{claim_closure} to generate the settlement delays.
#' \cr \cr
#' **Case 2: claims with less than 4 partial payments.** Proceed as in Case 1
#' but without separating out the simulation of the last payment delay (i.e.
#' ignore step 1).
#' @examples
#' # set up
#' n_vector <- claim_frequency(I = 10)
#' claim_sizes <- claim_size(n_vector)
#' no_payments <- claim_payment_no(n_vector, claim_sizes)
#' setldel <- claim_closure(n_vector, claim_sizes)
#'
#' # with default setting
#' pmtdel <- claim_payment_delay(n_vector, claim_sizes, no_payments, setldel)
#' pmtdel[[1]][[1]] # payment delays for claim 1 of occurrence period 1
#' @export
claim_payment_delay <- function(
  frequency_vector,
  claim_size_list,
  no_payments_list,
  settlement_list,
  settlement_mean_function,
  simulate_delay_function) {

  # we use the same default mean function as in claim_closure()
  if (missing(settlement_mean_function)) {
    settlement_mean_function <- function(claim_size, occurrence_period) {
      if (claim_size < (0.10 * .pkgenv$ref_claim) & occurrence_period >= 21) {
        a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
      } else {
        a <- max(0.85, 1 - 0.0075 * occurrence_period)
      }

      mean_quarter <- a * min(25, max(1, 6 + 4*log(claim_size/(0.10 * .pkgenv$ref_claim))))
      return(mean_quarter / 4 / .pkgenv$time_unit)
    }
  }

  # default simulate_delay_function
  if (missing(simulate_delay_function)) {
    simulate_delay_function <- function(no_pmt, claim_size, setldel, occurrence_period,
                                        setldel_mean_function) {
      result <- c(rep(NA, no_pmt))

      # First simulate the unnormalised values of d, sampled from a Weibull distribution
      if (no_pmt >= 4) {
        # 1) Simulate the last payment delay
        unnorm_d_mean <- (1 / 4) / .pkgenv$time_unit
        unnorm_d_cv <- 0.20
        parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
        result[no_pmt] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])

        # 2) Simulate all the other payment delays
        for (i in 1:(no_pmt - 1)) {
          unnorm_d_mean <- setldel_mean_function(claim_size = claim_size,
                                                 occurrence_period = occurrence_period)/no_pmt
          unnorm_d_cv <- 0.35
          parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
          result[i] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])
        }

      } else {
        for (i in 1:no_pmt) {
          unnorm_d_mean <- setldel_mean_function(claim_size = claim_size,
                                                 occurrence_period = occurrence_period)/no_pmt
          unnorm_d_cv <- 0.35
          parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
          result[i] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])
        }
      }

      stopifnot(sum(is.na(result)) == 0)
      # Normalise d such that sum(inter-partial delays) = settlement delay
      # To make sure that the pmtdels add up exactly to setldel, we treat the last one separately
      result[1:no_pmt-1] <- (setldel/sum(result)) * result[1:no_pmt-1]
      result[no_pmt] <- setldel - sum(result[1:no_pmt-1])

      return(result)
    }
  }

  I <- length(frequency_vector)
  payment_delays <- vector("list", I)
  for (i in 1:I) {
    payment_delays[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      payment_delays[[i]][[j]] <- simulate_delay_function(no_pmt = no_payments_list[[i]][j],
                                                          claim_size = claim_size_list[[i]][j],
                                                          setldel = settlement_list[[i]][j],
                                                          occurrence_period = i,
                                                          setldel_mean_function = settlement_mean_function)
    }
  }
  payment_delays
}


#' Partial Payment Times (in Continuous Time Scale)
#'
#' Converts the list of inter-partial delays to a list of payment times in
#' continuous time scale. Set `discrete = TRUE` to get the payment times in
#' calendar periods.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param occurrence_list list of claim occurrence times.
#' @param notification_list list of notification delays.
#' @param payment_delay_list (compound) list of inter partial delays.
#' @param discrete logical; if TRUE returns integer-valued payment times
#' (default FALSE).
#'
#' @details Returns a compound list structure such that the \eqn{j}th component
#' of the \eqn{i}th sub-list gives the payment time pattern (as a vector) for
#' the \eqn{j}th claim of occurrence period \eqn{i}. \cr \cr
#' Note that, as in the case of \code{\link{claim_closure}}, this function can
#' result in out-of-bound payment dates (i.e. payment times beyond the maximum
#' number of development periods under consideration). In these cases, we retain
#' the original simulated values for the simulation of other quantities, but we
#' will make adjustments for such claims in the tabulation of results in
#' \code{\link{claim_output}} and the payment inflation function
#' \code{\link{claim_payment_inflation}}.
#' @export
claim_payment_time <- function(
  frequency_vector, occurrence_list, notification_list, payment_delay_list,
  discrete = FALSE) {

  I <- length(frequency_vector)
  payment_times <- vector("list", I)
  for (i in 1:I) {
    payment_times[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      payment_times[[i]][[j]] <- occurrence_list[[i]][j] + notification_list[[i]][j] + cumsum(payment_delay_list[[i]][[j]])
    }
  }

  if (discrete == TRUE) {
    payment_times <- lapply(payment_times, lapply, ceiling)
  }
  payment_times
}

