###############################################################################
##                             8. Claim inflation                            ##
###############################################################################

#' Size of Partial Payments (With Inflation)
#'
#' Converts the (compound) list of constant-dollar-value payment sizes to a
#' (compound) list of inflated payment sizes by applying inflation rates on a
#' continuous time scale. \cr \cr
#' Compare with \code{claim_payment_size()} which generates the constant dollar
#' amount of partial payment sizes. Note that the constant dollar values are as
#' of time 0.
#'
#' @param frequency_vector a vector of claim frequencies for all the occurrence
#' periods.
#' @param payment_size_list (compound) list of payment size pattern (without
#' inflation).
#' @param payment_time_list (compound) list of payment times on a **continuous**
#' time scale.
#' @param occurrence_list (compound) list of occurrence times on a
#' **continuous** time scale.
#' @param claim_size_list list of claim sizes.
#' @param base_inflation_vector vector showing **quarterly** base inflation
#' rates (quarterly effective) for all the periods under consideration (default
#' at nil base inflation).
#' @param si_occurrence_function function of \code{occurrence_time} and
#' \code{claim_size} that outputs the superimposed inflation index with respect
#' to claim occurrence time (see Details for the default inflation function).
#' @param si_payment_funtion function of \code{payment_time} and
#' \code{claim_size} that outputs the superimposed inflation index with respect
#' to payment time (see Details for the default inflation function).
#'
#' @details Returns a compound list structure such that the \eqn{j}th component
#' of the \eqn{i}th sub-list gives the **inflated** payment pattern (as a
#' vector) for the \eqn{j}th claim of occurrence period \eqn{i}. \cr \cr
#' By default we assume
#' * Nil base inflation.
#' * No superimposed inflation by (continuous) occurrence time for the first 20
#' quarters (converted to the relevant `time_unit`); beyond 20 quarters, the
#' inflation index is given by
#' \deqn{1 - 0.4 max(0, 1 - claim_size/(0.25 * ref_claim))}
#' where `ref_claim` is a package-wise global variable that user is required to
#' define at the top of their code using \code{\link{set_parameters}}. The
#' interpretation is that, due to some external change to the insurance scheme
#' at the end of occurrence quarter 20, the smallest claims will reduce by up to
#' 40% in size. This change will not impact claims exceeding `0.25*ref_claim` in
#' size. The reduction varies linearly between these claim sizes.
#' * Superimposed inflation by (continuous) payment time operates at a period
#' rate of \deqn{\gamma * max(0, 1 - claim_size/ref_claim)} where \eqn{\gamma} is
#' equivalent to a 30% p.a. inflation rate (converted to the relevant
#' `time_unit`). The interpretation is that, for claims of small size the
#' payment time superimposed inflation tends to be very high (30% p.a.);
#' whereas for claims exceeding `ref_claim` in dollar values as of \eqn{t = 0},
#' the payment time superimposed inflation is nil. The rate of inflation
#' varies linearly between claim sizes of zero and `ref_claim`. \cr \cr
#'
#' **Remark on continuous inflation**: We note that `SynthETIC` works with
#' exact transaction times, so time has been measured continuously throughout
#' the program. This allows us to apply inflation on a continous time scale too.
#' For example, we asked the users to provide base inflation as a vector of
#' quarterly base inflation rates, quarterly effective for all the periods under
#' consideration. This data is generally available online (e.g. the Australian
#' quarterly inflation is available on RBA's website - see
#' [link](https://www.rba.gov.au/inflation/measures-cpi.html#quarterly)). We
#' then interpolate the quarterly inflation rates to compute the addition of
#' inflation by exact times. In the case of above, if we observed quarterly
#' inflation rates of 0.6%, 0.5%, 0.7% and 0.3% for one particular year, then
#' the base inflation applied to a payment at time \eqn{t = 1.82} quarters will
#' be \eqn{1.006 * 1.005^{0.82}}.
#'
#' **Remark on out-of-bound payment times**: This function includes adjustment
#' for out-of-bound transaction dates, by forcing payments that were projected
#' to fall out of the maximum development period to be paid at the exact end of
#' the maximum development period allowed. For example, if we consider 40
#' periods of development and a claim incurred in the interval (20, 21] was
#' projected to have a payment at time 62.498210, then we would treat such a
#' payment as if it occurred at time 60 for the purpose of inflation.
#'
#' @examples
#' # remove SI occurrence and SI payment
#' SI_occurrence <- function(occurrence_time, claim_size) {1}
#' SI_payment <- function(payment_time, claim_size) {1}
#' # base inflation constant at 0.02 p.a. effective
#' # (length is 80 to cover the maximum time period)
#' base_inflation_vector <- rep((1 + 0.02)^(1/4) - 1, times = 80)
#' attach(test_claims_object)
#' payment_inflated_list <- claim_payment_inflation(
#'   frequency_vector, payment_size_list, payment_time_list,
#'   occurrence_list, claim_size_list, base_inflation_vector,
#'   SI_occurrence, SI_payment
#' )
#' detach(test_claims_object) # undo the attach
#' # inflated payments for claim 1 of occurrence period 1
#' payment_inflated_list[[1]][[1]]
#' @export
claim_payment_inflation <- function(
  frequency_vector,
  payment_size_list,
  payment_time_list,
  occurrence_list,
  claim_size_list,
  base_inflation_vector,
  si_occurrence_function,
  si_payment_funtion) {

  I <- length(frequency_vector)
  # convert to number of calendar quarters (floor should be unnecessary)
  # times 2 to get the maximum calendar period
  no_quarters <- floor(I * .pkgenv$time_unit * 4) * 2

  # base inflation, default nil
  if (missing(base_inflation_vector)) {
    base_inflation_vector <- rep(0, times = no_quarters)
  }
  # convert the supplied inflation rates to a continuous index function
  # note that q is in quarters, so when we apply this function we need to convert time to quarters
  base_inflation <- function(q) {
    if (q == 0) {
      return(1)
    }
    # rate to index
    # e.g. (0.03, 0.01, 0.02) is converted to (1.03, 1.03*1.01, 1.03*1.01*1.02)
    base_inflation_index <- cumprod(1 + base_inflation_vector)
    # assume continous compounding within quarters
    # e.g. if q = 1.82, returns 1.03 * 1.01^(0.82)
    max(base_inflation_index[floor(q)], 1) * (1 + base_inflation_vector[ceiling(q)])^(q - floor(q))
  }
  base_inflation <- Vectorize(base_inflation)

  # default superimposed inflation functions
  # 1) with respect to occurrence time
  if (missing(si_occurrence_function)) {
    si_occurrence_function <- function(occurrence_time, claim_size) {
      if (occurrence_time <= 20 / 4 / .pkgenv$time_unit) {1}
      else {1 - 0.4*max(0, 1 - claim_size/(0.25 * .pkgenv$ref_claim))}
    }
  }
  # 2) with respect to payment time
  if (missing(si_payment_funtion)) {
    si_payment_funtion <- function(payment_time, claim_size) {
      period_rate <- (1 + 0.30)^(.pkgenv$time_unit) - 1
      beta <- period_rate * max(0, 1 - claim_size/.pkgenv$ref_claim)
      (1 + beta)^payment_time
    }
  }

  payment_inflated <- vector("list", I)
  adjustment <- 0
  for (i in 1:I) {
    payment_inflated[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      total_claim_size <- claim_size_list[[i]][j]
      t <- payment_time_list[[i]][[j]] # note t is a vector
      occr_time <- occurrence_list[[i]][j]
      # ADJUSTMENT FOR OUT-OF-BOUND TRANSACTION TIMES
      # e.g. If I = 40, a claim that was incurred during period 1 (between time
      # 0 and 1) must be closed before t = 40
      if (any(t > i + I - 1)) {
        adjustment <- adjustment + sum(t > i + I - 1)
        t[which(t > i + I - 1)] <- i + I - 1
      }
      base <- base_inflation(t * .pkgenv$time_unit * 4) / base_inflation(0)
      si_occurrence <- si_occurrence_function(occr_time, total_claim_size) / si_occurrence_function(0, total_claim_size)
      si_payment <- si_payment_funtion(t, total_claim_size) / si_payment_funtion(0, total_claim_size)

      # note that * does elementwise multiplication
      payment_inflated[[i]][[j]] <- payment_size_list[[i]][[j]] * base * si_occurrence * si_payment
    }
  }

  total_pmt_count <- length(unlist(payment_inflated))
  if (adjustment / total_pmt_count > 0.03) {
    warning("More than 3% of the payments were outside the bound.
    Check your notification and/or settlement delay assumptions!")
  }
  payment_inflated
}
