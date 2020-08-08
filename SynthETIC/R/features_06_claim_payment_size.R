###############################################################################
##            6. Sizes of Partial payments (constant dollar values)          ##
###############################################################################

#' Size of Partial Payments (Without Inflation)
#'
#' Simulates and returns the constant dollar amount of each partial payment
#' (i.e.**without inflation**) for each of the claims occurring in each of
#' the periods.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param claim_size_list list of claim sizes.
#' @param no_payments_list list of number of partial payments.
#' @param simulate_amt_pmt_function a function that generates and returns the
#' payment pattern of a particular claim (as a vector of size = \code{no_pmt}),
#' conditional on \code{no_pmt} and \code{claim_size} (see Details for the
#' default simulation function).
#' @details Returns a compound list structure such that the \eqn{j}th component
#' of the \eqn{i}th sub-list gives the payment pattern (as a vector) for the
#' \eqn{j}th claim of occurrence period \eqn{i}. \cr \cr
#' The default \code{simulate_amt_pmt_function} is set up in three steps. First
#' we sample the *complement* of the porportion of total claim size
#' represented by the last two payments, from a Beta distribution with mean
#' \deqn{1 - min(0.95, 0.75 + 0.04log[claim_size/(0.10 * ref_claim)])} where
#' `ref_claim` is a package-wise global variable that we ask the user to define
#' at the top of their code using \code{\link{set_parameters}}. CoV is assumed
#' constant at 20%.
#' \cr \cr
#' Next we simulate the porportion of last_two_pmts paid in the second last
#' payment (*settlement of the claim*) from a Beta distribution with
#' mean = 0.90 and CoV = 3%.
#' \cr \cr
#' Lastly we sample the remaining payment proportions from a Beta distribution
#' with mean \deqn{(1 - last_two_payments)/(no_pmt - 2)} and CoV = 10%, which
#' is followed by a normalisation such that the proportions add up to 1.
#' \cr \cr
#' In the cases where there are only 2 or 3 partial payments, proceed as if
#' there were 4 or 5 payments respectively with last_two_payments = 0. The
#' trivial case is when the claim is settled with a single payment, which must
#' be of the same amount as the total claim size.
#' @section Explanation: Why did we set up a payment pattern as above? \cr \cr
#' The payment pattern is set up to reflect the typical pattern of a claim from
#' an Auto liability line of business, which usually consists of:
#' \enumerate{
#' \item (possibly) some small payments such as police reports, medical
#' consultations and reports;
#' \item some more substantial payments such as hospitalisation, specialist
#' medical procedures, equipment (e.g. prosthetics);
#' \item a final settlement with the claimant (usually the second last payment);
#' \item a smaller final payment, usually covering legal costs.
#' }
#' Claims in a number of other lines of business exhibit a similar structure,
#' albeit with possible differences in the types of payment made.
#' @examples
#' # set up
#' n_vector <- claim_frequency(I = 10)
#' claim_sizes <- claim_size(n_vector)
#' no_payments <- claim_payment_no(n_vector, claim_sizes)
#'
#' # with default simulate_amt_pmt_function
#' payments <- claim_payment_size(n_vector, claim_sizes, no_payments)
#' # partial payment sizes for claim 1 of occurrence period 1
#' payments[[1]][[1]]
#'
#' # with some custom simulate_amt_pmt_function
#' # simplest case: (stochastically) equal amounts
#' my_func <- function(no_pmt, claim_size) {
#'   prop <- runif(no_pmt)
#'   prop <- prop/sum(prop)
#'   claim_size * prop
#' }
#' mypayments <- claim_payment_size(n_vector, claim_sizes, no_payments, my_func)
#' # partial payment sizes for claim 1 of occurrence period 1
#' mypayments[[1]][[1]]
#' @export
claim_payment_size <- function(
  frequency_vector,
  claim_size_list,
  no_payments_list,
  simulate_amt_pmt_function) {

  # default simulate_amt_pmt_function
  if (missing(simulate_amt_pmt_function)) {
    simulate_amt_pmt_function <- function(no_pmt, claim_size) {
      if (no_pmt >= 4) {
        ## 1) Simulate the "complement" of the proportion of total claim size
        ##    represented by the last two payments
        p_mean <- 1 - min(0.95, 0.75 + 0.04*log(claim_size/(0.10 * .pkgenv$ref_claim)))
        p_CV <- 0.20
        p_parameters <- get_Beta_parameters(target_mean = p_mean, target_cv = p_CV)
        last_two_pmts_complement <- stats::rbeta(1, shape1 = p_parameters[1], shape2 = p_parameters[2])
        last_two_pmts <- 1 - last_two_pmts_complement

        ## 2) Simulate the proportion of last_two_pmts paid in the second last payment
        q_mean <- 0.9
        q_CV <- 0.03
        q_parameters <- get_Beta_parameters(target_mean = q_mean, target_cv = q_CV)
        q <- stats::rbeta(1, shape1 = q_parameters[1], shape2 = q_parameters[2])

        ## 3) Calculate the respective proportions of claim amount paid in the last 2 payments
        p_second_last <- q * last_two_pmts
        p_last <- (1-q) * last_two_pmts

        ## 4) Simulate the "unnormalised" proportions of claim amount paid in the first (m - 2) payments
        p_unnorm_mean <- last_two_pmts_complement/(no_pmt - 2)
        p_unnorm_CV <- 0.10
        p_unnorm_parameters <- get_Beta_parameters(target_mean = p_unnorm_mean, target_cv = p_unnorm_CV)
        amt <- stats::rbeta(no_pmt - 2, shape1 = p_unnorm_parameters[1], shape2 = p_unnorm_parameters[2])

        ## 5) Normalise the proportions simulated in step 4
        amt <- last_two_pmts_complement * (amt/sum(amt))

        ## 6) Attach the last 2 proportions, p_second_last and p_last
        amt <- append(amt, c(p_second_last, p_last))

        ## 7) Multiply by claim_size to obtain the actual payment amounts
        amt <- claim_size * amt

      } else if (no_pmt == 2 | no_pmt == 3) {
        p_unnorm_mean <- 1/no_pmt
        p_unnorm_CV <- 0.10
        p_unnorm_parameters <- get_Beta_parameters(target_mean = p_unnorm_mean, target_cv = p_unnorm_CV)
        amt <- stats::rbeta(no_pmt, shape1 = p_unnorm_parameters[1], shape2 = p_unnorm_parameters[2])

        ## Normalise the proportions and multiply by claim_size to obtain the actual payment amounts
        amt <- claim_size * amt/sum(amt)

      } else {
        # when there is a single payment
        amt <- claim_size

      }

      stopifnot(length(amt) == no_pmt)
      return(amt)
    }
  }

  I <- length(frequency_vector)
  payment_sizes <- vector("list", I)
  for (i in 1:I) {
    payment_sizes[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      payment_sizes[[i]][[j]] <- simulate_amt_pmt_function(no_pmt = no_payments_list[[i]][j],
                                                           claim_size = claim_size_list[[i]][j])
    }
  }

  payment_sizes
}
