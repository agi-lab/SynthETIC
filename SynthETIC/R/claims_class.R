###############################################################################
##                  The "claims" class and helper functions                  ##
###############################################################################

#' Construction of a `claims` Object
#'
#' Contructs a `claims` object which stores all the simulated quantities.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param occurrence_list list of claim occurrence times.
#' @param claim_size_list list of claim sizes.
#' @param notification_list list of notification delays.
#' @param settlement_list list of settlement delays.
#' @param no_payments_list list of number of partial payments.
#' @param payment_size_list (compound) list of payment size pattern (without
#' inflation).
#' @param payment_delay_list (compound) list of inter partial delays.
#' @param payment_time_list (compound) list of payment times on a **continuous**
#' time scale.
#' @param payment_inflated_list (compound) list of payment size pattern (after
#' inflation).
#'
#' @return Returns a `claims` object (mainly a reformat of the arguments as a
#' list object), with the 10 components as listed above.
#' @export
claims <- function(
  frequency_vector,
  occurrence_list,
  claim_size_list,
  notification_list,
  settlement_list,
  no_payments_list,
  payment_size_list,
  payment_delay_list,
  payment_time_list,
  payment_inflated_list
) {

  claims <- list(
    frequency_vector = frequency_vector,
    occurrence_list = occurrence_list,
    claim_size_list = claim_size_list,
    notification_list = notification_list,
    settlement_list = settlement_list,
    no_payments_list = no_payments_list,
    payment_size_list = payment_size_list,
    payment_delay_list = payment_delay_list,
    payment_time_list = payment_time_list,
    payment_inflated_list = payment_inflated_list
  )

  attr(claims, "class") <- "claims"
  claims
}
