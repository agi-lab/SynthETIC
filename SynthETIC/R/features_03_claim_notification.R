###############################################################################
##                           3. Claim notification                           ##
###############################################################################

#' Claim Notification
#'
#' Simulates and returns the notification/reporting delays of each of the
#' claims occurring in each of the periods, according to a user-specified
#' distribution (by default a Weibull).
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param claim_size_list list of claim sizes.
#' @param rfun optional alternative random sampling function; if not specified,
#' assumes Weibull.
#' @param paramfun parameters for the random sampling function, as a function of
#' \code{claim_size} and \code{occurrence_period}; see Details.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' @return A list of notification delays such that the \eqn{i}th component of
#' the list gives the notification delays for all claims that occurred in
#' period \eqn{i}.
#'
#' @details Claim notification delay represents the delay from claim occurrence
#' to reporting. `SynthETIC` assumes the (removable) dependence of notification
#' delay on claim size and occurrence period of the claim, thus requiring the
#' user to specify a `paramfun` of `claim_size` and `occurrrence_period` (with
#' the option to add more arguments as needed).
#'
#' The `paramfun` should return the distribution parameters in a **vector**,
#' e.g. for gamma distribution `paramfun` should return a value in the format of
#' `c(shape = , scale = )`, for exponential distribution this should return
#' `c(rate = )`. See Examples. If a `rfun` is specified without a `paramfun`,
#' `SynthETIC` will try to proceed without parameterisation (i.e. directly
#' calling `rfun` with claim size and occurrence period), and if it fails,
#' return an error message.
#'
#' By default, it is assumed that the notification delay follows a Weibull
#' distribution, and that the mean notification delay (in quarters) is given by
#' \deqn{min(3, max(1, 2-[log(claim_size/(0.50*ref_claim))]/3))}
#' automatically converted to the relevant `time_unit` defined by user at the
#' top of their script through \code{\link{set_parameters}}. Note that the
#' `ref_claim` in the equation is another package-wise global variable that the
#' user needs to define through \code{\link{set_parameters}} as it determines
#' the monetary scale of the simulator. The CoV (Coefficient of Variation)
#' of the notification delay is assumed to be constant at 70%, independent of
#' the size and occurrence period of the claim.
#'
#' Of course, the user may wish to sample from a different distribution `rfun`
#' and/or a different set of parameters. An example is given below.
#'
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#'
#' # Try a constant Weibull distribution
#' # (i.e. independent of claim size and occurrence period)
#' notidel_param <- function(claim_size, occurrence_period) {
#'   mean <- 2; cv <- 0.70
#'   shape <- get_Weibull_parameters(mean, cv)[1, ]
#'   scale <- get_Weibull_parameters(mean, cv)[2, ]
#'   c(shape = shape, scale = scale)
#' }
#'
#' notidel <- claim_notification(n_vector, claim_size(n_vector),
#'                               paramfun = notidel_param)
#' notidel[[1]] # show notification for claims originating from period 1
#' @export
claim_notification <- function(
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
        # not expecting users to remove the two function arguments, they are
        # included to remind the user of the SynthETIC dependence structure
        #
        # they may, however, add more arguments to the paramfun
        mean <- min(3, max(1, 2-(log(claim_size/(0.50 * .pkgenv$ref_claim)))/3))/4 / .pkgenv$time_unit
        cv <- 0.70

        shape <- get_Weibull_parameters(mean, cv)[1, ]
        scale <- get_Weibull_parameters(mean, cv)[2, ]

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
    tt <- try(notidel_vect <- do.call(rfun, keep_formals), TRUE)
    if (methods::is(tt, "try-error")) {
      stop("need to specify 'paramfun' for the sampling distribution")
    }
  } else {
    notidel_vect <- do.call(rfun, keep_formals)
  }

  notidel <- to_SynthETIC(notidel_vect, frequency_vector)
  notidel
}
