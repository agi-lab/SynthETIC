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
#' @param rfun optional alternative random sampling function; see Details for
#' default.
#' @param paramfun parameters for the random sampling function, as a function of
#' \code{claim_size}; see Details.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' @details Returns a compound list structure such that the \eqn{j}th component
#' of the \eqn{i}th sub-list gives the payment pattern (as a vector) for the
#' \eqn{j}th claim of occurrence period \eqn{i}.
#'
#' The default \code{rfun} is set up in three steps. First
#' we sample the **complement** of the proportion of total claim size
#' represented by the last two payments, from a Beta distribution with mean
#' \deqn{1 - min(0.95, 0.75 + 0.04log[claim_size/(0.10 * ref_claim)])} where
#' `ref_claim` is a package-wise global variable that we ask the user to define
#' at the top of their code using \code{\link{set_parameters}}. CoV is assumed
#' constant at 20%.
#'
#' Next we simulate the proportion of last_two_pmts paid in the second last
#' payment (*settlement of the claim*) from a Beta distribution with
#' mean = 0.90 and CoV = 3%.
#'
#' Lastly we sample the remaining payment proportions from a Beta distribution
#' with mean \deqn{(1 - last_two_payments)/(no_pmt - 2)} and CoV = 10%, which
#' is followed by a normalisation such that the proportions add up to 1.
#'
#' In the cases where there are only 2 or 3 partial payments, proceed as if
#' there were 4 or 5 payments respectively with last_two_payments = 0. The
#' trivial case is when the claim is settled with a single payment, which must
#' be of the same amount as the total claim size.
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
#' @section Explanation: Why did we set up a payment pattern as above?
#'
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
#'
#' @examples
#' # set up
#' n_vector <- claim_frequency(I = 10)
#' claim_sizes <- claim_size(n_vector)
#' no_payments <- claim_payment_no(n_vector, claim_sizes)
#'
#' # with default rfun
#' payments <- claim_payment_size(n_vector, claim_sizes, no_payments)
#' # partial payment sizes for claim 1 of occurrence period 1
#' payments[[1]][[1]]
#'
#' # with some custom rfun
#' # simplistic case: (stochastically) equal amounts
#' my_func <- function(n, claim_size) {
#'   prop <- runif(n)
#'   prop <- prop / sum(prop)
#'   claim_size * prop
#' }
#' mypayments <- claim_payment_size(n_vector, claim_sizes, no_payments, my_func)
#' # partial payment sizes for claim 1 of occurrence period 1
#' mypayments[[1]][[1]]
#' @export
claim_payment_size <- function(
  frequency_vector, claim_size_list, no_payments_list, rfun, paramfun, ...) {

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

  # default simulation function for the sizes of partial payments
  # we run rfun on each individual claim, and output a vector of payment sizes
  if (missing(rfun)) {
    rfun <- function(n, claim_size) {
      # n = number of simulations, here n should be the number of partial payments
      if (n >= 4) {
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
        p_unnorm_mean <- last_two_pmts_complement/(n - 2)
        p_unnorm_CV <- 0.10
        p_unnorm_parameters <- get_Beta_parameters(target_mean = p_unnorm_mean, target_cv = p_unnorm_CV)
        amt <- stats::rbeta(n - 2, shape1 = p_unnorm_parameters[1], shape2 = p_unnorm_parameters[2])

        ## 5) Normalise the proportions simulated in step 4
        amt <- last_two_pmts_complement * (amt/sum(amt))

        ## 6) Attach the last 2 proportions, p_second_last and p_last
        amt <- append(amt, c(p_second_last, p_last))

        ## 7) Multiply by claim_size to obtain the actual payment amounts
        amt <- claim_size * amt

      } else if (n == 2 | n == 3) {
        p_unnorm_mean <- 1 / n
        p_unnorm_CV <- 0.10
        p_unnorm_parameters <- get_Beta_parameters(target_mean = p_unnorm_mean, target_cv = p_unnorm_CV)
        amt <- stats::rbeta(n, shape1 = p_unnorm_parameters[1], shape2 = p_unnorm_parameters[2])

        ## Normalise the proportions and multiply by claim_size to obtain the actual payment amounts
        amt <- claim_size * amt/sum(amt)

      } else {
        # when there is a single payment
        amt <- claim_size

      }

      stopifnot(length(amt) == n)
      return(amt)
    }

    # the default rfun directly takes claim_size as an input, so the "empty"
    # paramfun would do the trick
    paramfun <- function(...) {
      c(...)
    }
  }


  I <- length(frequency_vector)
  no_claims <- sum(frequency_vector)
  no_pmt_vector <- unlist(no_payments_list, use.names = FALSE)
  params <- mapply(paramfun,
                   claim_size = unlist(claim_size_list, use.names = FALSE),
                   ...)

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

  payment_sizes <- vector("list", I)
  k <- 1
  for (i in 1:I) {
    payment_sizes[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      if (paramfun_filled) {
        tt <- try(payment_sizes[[i]][[j]] <- do.call(rfun, as.list(args_df[, k])))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun' for the sampling distribution")
        }
      } else {
        payment_sizes[[i]][[j]] <- do.call(rfun, as.list(args_df[, k]))
      }

      k <- k + 1 # k tracks the claim number
    }
  }

  payment_sizes
}
