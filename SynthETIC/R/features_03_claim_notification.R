###############################################################################
##                           3. Claim notification                           ##
###############################################################################

#' Claim Notification
#'
#' Simulates and returns the notification/reporting delays of each of the
#' claims occurring in each of the periods, assuming a Weibull distribution.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param claim_size_list list of claim sizes.
#' @param mean_function target mean of the Weibull distribution as a function
#' of \code{claim_size} and \code{occurrence_period} (see Details for default
#' mean function).
#' @param cv_function target CoV (Coefficient of Variation) of the Weibull
#' distribution as a function of \code{claim_size} and \code{occurrence_period}
#' (see Details for default CoV function).
#' @return A list of notification delays such that the \eqn{i}th component of
#' the list gives the notification delays for all claims that occurred in
#' period \eqn{i}.
#' @details Claim notification delay represents the delay from claim occurrence
#' to claim reporting. It is assumed that the notification delay follows a
#' Weibull distribution with parameters possibly dependent on claim size and
#' occurrence period. \cr \cr
#' By default, we assume that mean notification delay (in quarters) is given by
#' \deqn{min(3, max(1, 2-[log(claim_size/(0.50*ref_claim))]/3))}
#' automatically converted to the relevant `time_unit` defined by user at the
#' top of their script through \code{\link{set_parameters}}. Note that the
#' `ref_claim` in the equation is another package-wise global variable that the
#' user needs to define through \code{\link{set_parameters}} as it determines
#' the monetary scale of the simulator. The CoV (Coefficient of Variation)
#' of the notification delay is assumed to be constant at 70%, independent of
#' the size and occurrence period of the claim.
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#'
#' # Try constant mean/CoV function (i.e. independent of claim size)
#' notidel_mean <- function(claim_size, occurrence_period) {2}
#' notidel_cv <- function(claim_size, occurrence_period) {0.70}
#'
#' notidel <- claim_notification(n_vector, claim_size(n_vector),
#'                               notidel_mean, notidel_cv)
#' notidel[[1]] # show notification for claims originating from period 1
#' @export
claim_notification <- function(frequency_vector, claim_size_list, mean_function, cv_function) {
  # default mean_function and cv_function
  if (missing(mean_function)) {
    mean_function <- function(claim_size, occurrence_period) {
      min(3, max(1, 2-(log(claim_size/(0.50 * .pkgenv$ref_claim)))/3))/4 / .pkgenv$time_unit
    }
  }
  if (missing(cv_function)) {
    cv_function <- function(claim_size, occurrence_period) {0.70}
  }

  # vectorise mean_function and cv_function so they apply to vector arguments
  mean_function <- Vectorize(mean_function)
  cv_function <- Vectorize(cv_function)

  I <- length(frequency_vector)
  notidel <- vector("list", I)
  for (i in 1:I) {
    s_i <- claim_size_list[[i]] # s_i gives the claim sizes for all claims that occurred in period i
    parameters <- get_Weibull_parameters(target_mean = mean_function(claim_size = s_i, occurrence_period = i),
                                         target_cv = cv_function(claim_size = s_i, occurrence_period = i))
    notidel[[i]] <- stats::rweibull(frequency_vector[i], shape = parameters[1, ], scale = parameters[2, ])
  }
  notidel
}
