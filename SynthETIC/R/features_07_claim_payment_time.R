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
#' @param rfun optional alternative random sampling function; see Details for
#' default.
#' @param paramfun parameters for the random sampling function, as a function of
#' \code{claim_size}, \code{setldel}; see Details.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' @details Returns a compound list structure such that the \eqn{j}th component
#' of the \eqn{i}th sub-list gives the payment delay pattern (as a vector) for
#' the \eqn{j}th claim of occurrence period \eqn{i}.
#'
#' The default \code{rfun} is split into 2 cases.
#'
#' **Case 1: claims with at least 4 partial payments.** The simulation takes
#' 2 steps.
#' First we sample the last payment delay from a Weibull distribution with
#' mean = 1 quarter (automatically converted to the relevant `time_unit`, a
#' global variable that the user is required to define at the top of their code)
#' and CoV = 20%. Then we sample the remaining payment delays from a second
#' Weibull distribution with CoV at 35% and mean = target mean settlement delay
#' (see \code{\link{claim_closure}}) divided by the number of payments.
#'
#' **Case 2: claims with less than 4 partial payments.** Proceed as in Case 1
#' but without separating out the simulation of the last payment delay (i.e.
#' ignore step 1).
#'
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
#'
#' # with some custom rfun
#' # simplistic case: payments times are uniformly distributed
#' my_func <- function(n, setldel) {
#'   prop <- runif(n)
#'   prop <- prop / sum(prop)
#'   setldel * prop
#' }
#' mypayments <- claim_payment_delay(n_vector, claim_sizes, no_payments, setldel,
#'                                   rfun = my_func)
#' # inter-partial delays for claim 1 of occurrence period 1
#' mypayments[[1]][[1]]
#' @export
claim_payment_delay <- function(
  frequency_vector,
  claim_size_list,
  no_payments_list,
  settlement_list,
  rfun,
  paramfun,
  ...) {

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

  # default simulation function for the inter-partial delays of payments
  # we run rfun on each individual claim, and output a vector of payment delays
  use_default <- FALSE
  if (missing(rfun)) {

    # we will use this to determine whether occurrence_period needs to be used
    # in the application of paramfun
    use_default <- TRUE

    rfun <- function(n, claim_size, setldel, setldel_mean) {
      result <- c(rep(NA, n))

      # First simulate the unnormalised values of d, sampled from a Weibull distribution
      if (n >= 4) {
        # 1) Simulate the last payment delay
        unnorm_d_mean <- (1 / 4) / .pkgenv$time_unit
        unnorm_d_cv <- 0.20
        parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
        result[n] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])

        # 2) Simulate all the other payment delays
        for (i in 1:(n - 1)) {
          unnorm_d_mean <- setldel_mean / n
          unnorm_d_cv <- 0.35
          parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
          result[i] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])
        }

      } else {
        for (i in 1:n) {
          unnorm_d_mean <- setldel_mean / n
          unnorm_d_cv <- 0.35
          parameters <- get_Weibull_parameters(target_mean = unnorm_d_mean, target_cv = unnorm_d_cv)
          result[i] <- stats::rweibull(1, shape = parameters[1], scale = parameters[2])
        }
      }

      stopifnot(sum(is.na(result)) == 0)

      # Normalise d such that sum(inter-partial delays) = settlement delay
      # To make sure that the pmtdels add up exactly to setldel, we treat the last one separately
      result[1:n-1] <- (setldel/sum(result)) * result[1:n-1]
      result[n] <- setldel - sum(result[1:n-1])

      return(result)
    }

    paramfun_default <- function(claim_size, setldel, # required
                         occurrence_period # optional
    ) {
      # mean settlement delay
      if (claim_size < (0.10 * .pkgenv$ref_claim) & occurrence_period >= 21) {
        a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
      } else {
        a <- max(0.85, 1 - 0.0075 * occurrence_period)
      }
      mean_quarter <- a * min(25, max(1, 6 + 4*log(claim_size/(0.10 * .pkgenv$ref_claim))))
      target_mean <- mean_quarter / 4 / .pkgenv$time_unit

      c(claim_size = claim_size,
        setldel = setldel,
        setldel_mean = target_mean)
    }

  }

  I <- length(frequency_vector)
  no_claims <- sum(frequency_vector)
  no_pmt_vector <- unlist(no_payments_list, use.names = FALSE)

  if (use_default == TRUE) {
    params <- mapply(paramfun_default,
                     claim_size = unlist(claim_size_list, use.names = FALSE),
                     setldel = unlist(settlement_list, use.names = FALSE),
                     occurrence_period = rep(1:I, times = frequency_vector),
                     ...)
  } else {
    params <- mapply(paramfun,
                     claim_size = unlist(claim_size_list, use.names = FALSE),
                     setldel = unlist(settlement_list, use.names = FALSE),
                     ...)
  }

  # if params only has one parameter, asplit() won't work
  if (!is.null(names(params))) {
    params_split <- split(unname(params), names(params))
  } else if (length(params)) {
    params_split <- asplit(params, 1)
  } else {
    params_split <- params
  }

  # do.call rfun, but ignore unused arguments
  args <- as.list(params_split)
  keep_names <- c(intersect(names(args), names(formals(rfun))))
  keep_formals <- c(list(n = no_pmt_vector), args[keep_names])

  # turn keep_formals, which is a list of arguments, to a dataframe
  args_df <- do.call(rbind, keep_formals)
  # in the dataframe, each row represents a parameter, and each column gives the
  # parameter values for a specific claim

  payment_delays <- vector("list", I)
  k <- 1
  for (i in 1:I) {
    payment_delays[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      if (paramfun_filled) {
        tt <- try(payment_delays[[i]][[j]] <- do.call(rfun, as.list(args_df[, k])))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun' for the sampling distribution")
        }
      } else {
        payment_delays[[i]][[j]] <- do.call(rfun, as.list(args_df[, k]))
      }

      k <- k + 1 # k tracks the claim number
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

