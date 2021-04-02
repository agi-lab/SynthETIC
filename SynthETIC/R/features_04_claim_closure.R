###############################################################################
##                              4. Claim closure                             ##
###############################################################################

#' Claim Closure
#'
#' Simulates and returns the closure/settlement delays of each of the claims
#' occurring in each of the periods, assuming a Weibull distribution.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param claim_size_list list of claim sizes.
#' @param rfun optional alternative random sampling function; if not specified,
#' assumes Weibull.
#' @param paramfun parameters for the random sampling function, as a function of
#' \code{claim_size} and \code{occurrence_period}; see Details.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' @return A list of settlement delays such that the \eqn{i}th component of
#' the list gives the settlement delays for all claims that occurred in period
#' \eqn{i}.
#'
#' @details Claim settlement delay represents the delay from claim notification
#' to closure. The epoch of closure is the sum of occurrence time, notification
#' delay and settlement delay.
#'
#' By default, it is assumed that the settlement delay follows a Weibull
#' distribution. The default Weibull parameters have been set up such that
#' the mean settlement delay (in quarters, but automatically converted to the
#' relevant `time_unit` as defined in \code{\link{set_parameters}}) is
#' porportional to
#' \deqn{min(25, max(1, 6 + 4 log[claim_size/(0.10 * ref_claim)]))} (where
#' `ref_claim` is a packagewise-global variable that user is required to define
#' by \code{\link{set_parameters}}) up to a scaling factor "\eqn{a}", which is
#' dependent on \code{occurrence_perid}. Specifically,
#' \deqn{a = min(0.85, 0.65 + 0.02 * (occurrence_period - 21))} if
#' \code{claim_size} < (0.10 * ref_claim) and \code{occurrence_period} \eqn{\ge}
#' 21, and \deqn{a = max(0.85, 1 - 0.0075 * occurrence_period)} otherwise.
#' The CoV of the settlement delay is constant at 60%, independent of the size
#' and occurrence period of the claim.
#'
#' Note that this function can create out-of-bound settlement dates. In these
#' cases, the simulated epoch of occurrence of the transaction is maintained
#' throughout the simulation of details of the claim concerned. Adjustments will
#' only be made for the tabulation of results in \code{\link{claim_output}} and
#' payment inflation.
#'
#' Of course, like any other \code{SynthETIC} modules, the user may wish to
#' sample from a different distribution `rfun` and/or a different set of
#' parameters. The `paramfun` should return the distribution parameters in a
#' **vector**, e.g. for gamma distribution `paramfun` should return a value in
#' the format of `c(shape = , scale = )`, for exponential distribution this
#' should return `c(rate = )`. See Examples. If a `rfun` is specified without
#' a `paramfun`, `SynthETIC` will try to proceed without parameterisation (i.e.
#' directly calling `rfun` with claim size and occurrence period), and if it
#' fails, then return an error message.
#'
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#'
#' # Try a constant Weibull distribution
#' # (i.e. independent of claim size and occurrence period)
#' setldel_param <- function(claim_size, occurrence_period) {
#'   mean <- 10; cv <- 0.70
#'   shape <- get_Weibull_parameters(mean, cv)[1, ]
#'   scale <- get_Weibull_parameters(mean, cv)[2, ]
#'   c(shape = shape, scale = scale)
#' }
#'
#' setldel <- claim_closure(n_vector, claim_size(n_vector),
#'                          paramfun = setldel_param)
#' setldel[[1]] # show settlement delay of claims originating from period 1
#' @export
claim_closure <- function(
  frequency_vector, claim_size_list, rfun, paramfun, ...) {

  if (!missing(rfun) & missing(paramfun)) {
    # we will see if we can continue without parameterisation
    # e.g. if the input rfun is directly a function of claim_size, and no
    # transformation is required
    paramfun <- function(claim_size, occurrence_period, ...) {
      c(claim_size = claim_size, occurrence_period = occurrence_period, ...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # default Weibull
  if (missing(rfun)) {
    # user can supply their own paramfun for weibull
    rfun <- stats::rweibull
    if (missing(paramfun)) {
      # or use the default specifications
      paramfun <- function(claim_size, occurrence_period, ...) {
        # specify the target Weibull mean
        if (claim_size < (0.10 * .pkgenv$ref_claim) & occurrence_period >= 21) {
          a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
        } else {
          a <- max(0.85, 1 - 0.0075 * occurrence_period)
        }
        mean_quarter <- a * min(25, max(1, 6 + 4*log(claim_size/(0.10 * .pkgenv$ref_claim))))
        target_mean <- mean_quarter / 4 / .pkgenv$time_unit

        # specify the target Weibull CoV
        target_cv <- 0.60

        shape <- get_Weibull_parameters(target_mean, target_cv)[1, ]
        scale <- get_Weibull_parameters(target_mean, target_cv)[2, ]
        c(shape = shape, scale = scale)
      }
    }
  }

  I <- length(frequency_vector)
  no_claims <- sum(frequency_vector)
  params <- mapply(paramfun,
                   claim_size = unlist(claim_size_list, use.names = FALSE),
                   occurrence_period = rep(1:I, times = frequency_vector),
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
    tt <- try(setldel_vect <- do.call(rfun, keep_formals), TRUE)
    if (methods::is(tt, "try-error")) {
      stop("need to specify 'paramfun' for the sampling distribution")
    }
  } else {
    setldel_vect <- do.call(rfun, keep_formals)
  }

  setldel <- to_SynthETIC(setldel_vect, frequency_vector)
  setldel
}
