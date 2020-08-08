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
#' @param mean_function target mean of the Weibull distribution as a function
#' of \code{claim_size} and \code{occurrence_period} (see Details for default
#' mean function).
#' @param cv_function target CoV of the Weibull distribution as a function of
#' \code{claim_size} and \code{occurrence_period} (see Details for default
#' CoV function).
#' @return A list of settlement delays such that the \eqn{i}th component of
#' the list gives the settlement delays for all claims that occurred in period
#' \eqn{i}.
#' @details Claim settlement delay represents the delay from claim notification
#' to closure. The epoch of closure is the sum of occurrence time, notification
#' delay and settlement delay. \cr \cr
#' It is assumed that the settlement delay follows a Weibull distribution with
#' parameters possibly dependent on claim size and occurrence period. \cr \cr
#' Recall that `ref_claim` is a packagewise-global variable that user is
#' required to define by \code{\link{set_parameters}}. By default, we assume
#' that mean settlement delay (in quarters, but automatically converted to the
#' relevant `time_unit` as defined in \code{\link{set_parameters}}) is
#' porportional to
#' \deqn{min(25, max(1, 6 + 4 log[claim_size/(0.10 * ref_claim)]))} up to a
#' scaling factor "\eqn{a}", which is dependent on \code{occurrence_perid}.
#' Specifically, \deqn{a = min(0.85, 0.65 + 0.02 * (occurrence_period - 21))} if
#' \code{claim_size} < (0.10 * ref_claim) and \code{occurrence_period} \eqn{\ge}
#' 21, and \deqn{a = max(0.85, 1 - 0.0075 * occurrence_period)} otherwise.
#' The CoV of the settlement delay is constant at 60%, independent of the size
#' and occurrence period of the claim. \cr \cr
#' Note that this function can create out-of-bound settlement dates. In these
#' cases, the simulated epoch of occurrence of the transaction is maintained
#' throughout the simulation of details of the claim concerned. Adjustments will
#' only be made for the tabulation of results in \code{\link{claim_output}} and
#' payment inflation.
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#'
#' # Try constant mean/CoV function (i.e. independent of claim size)
#' setldel_mean <- function(claim_size, occurrence_period) {10}
#' setldel_cv <- function(claim_size, occurrence_period) {0.70}
#'
#' setldel <- claim_closure(n_vector, claim_size(n_vector),
#'                          setldel_mean, setldel_cv)
#' setldel[[1]] # show settlement delay of claims originating from period 1
#' @export
claim_closure <- function(frequency_vector, claim_size_list, mean_function, cv_function) {
  # default mean_function and cv_function
  if (missing(mean_function)) {
    mean_function <- function(claim_size, occurrence_period) {
      if (claim_size < (0.10 * .pkgenv$ref_claim) & occurrence_period >= 21) {
        a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
      } else {
        a <- max(0.85, 1 - 0.0075 * occurrence_period)
      }

      mean_quarter <- a * min(25, max(1, 6 + 4*log(claim_size/(0.10 * .pkgenv$ref_claim))))
      return(mean_quarter / 4 / .pkgenv$time_unit)
    }
  }
  if (missing(cv_function)) {
    cv_function <- function(claim_size, occurrence_period) {0.60}
  }

  # vectorise mean_function and cv_function so they apply to vector arguments
  mean_function <- Vectorize(mean_function)
  cv_function <- Vectorize(cv_function)

  I <- length(frequency_vector)
  setldel <- vector("list", I)
  for (i in 1:I) {
    s_i <- claim_size_list[[i]] # s_i gives the claim sizes for all claims that occurred in period i
    parameters <- get_Weibull_parameters(target_mean = mean_function(claim_size = s_i, occurrence_period = i),
                                         target_cv = cv_function(claim_size = s_i, occurrence_period = i))
    setldel[[i]] <- stats::rweibull(frequency_vector[i], shape = parameters[1, ], scale = parameters[2, ])
  }
  setldel
}
