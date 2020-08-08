## code to prepare the test datasets goes here
## see the vignette for detailed comments on this code

set.seed(20200131)
set_parameters(ref_claim = 200000, time_unit = 1/4)
ref_claim <- return_parameters()[1]
time_unit <- return_parameters()[2]

###############################################################################
##                             1. Claim occurrence                           ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
years <- 10
I <- years / time_unit
E <- c(rep(12000, I))
lambda <- c(rep(0.03, I))
## ----------------------------------------------------------------------------

# Number of claims ocurring for each period i
n_vector <- claim_frequency(I, E, lambda)

# Occurrence time of each claim r, for each period i
occurrence_times <- claim_occurrence(n_vector)


###############################################################################
##                   2. Claim size (constant dollar values)                  ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
S_df <- function(s) {
  # truncate and rescale
  if (s < 30) {
    return(0)
  } else {
    p_trun <- pnorm(s^0.2, 9.5, 3) - pnorm(30^0.2, 9.5, 3)
    p_rescaled <- p_trun/(1 - pnorm(30^0.2, 9.5, 3))
    return(p_rescaled)
  }
}
## ----------------------------------------------------------------------------

claim_sizes <- claim_size(n_vector, S_df, range = c(0, 1e24))


###############################################################################
##                           3. Claim notification                           ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
notidel_mean <- function(claim_size, occurrence_period) {
  # WARNING: Do not change function arguments
  min(3, max(1, 2-(log(claim_size/(0.50 * ref_claim)))/3))/4 / time_unit
}

notidel_cv <- function(claim_size, occurrence_period) {
  # WARNING: Do not change function arguments
  0.70
}
## ----------------------------------------------------------------------------

notidel <- claim_notification(n_vector, claim_sizes, notidel_mean, notidel_cv)


###############################################################################
##                              4. Claim closure                             ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
setldel_mean <- function(claim_size, occurrence_period) {
  # WARNING: Do not change function arguments
  if (claim_size < (0.10 * ref_claim) & occurrence_period >= 21) {
    a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
  } else {
    a <- max(0.85, 1 - 0.0075 * occurrence_period)
  }

  mean_quarter <- a * min(25, max(1, 6 + 4*log(claim_size/(0.10 * ref_claim))))
  return(mean_quarter / 4 / time_unit)
}

setldel_cv <- function(claim_size, occurrence_period) {
  # WARNING: Do not change function arguments
  0.60
}
## ----------------------------------------------------------------------------

setldel <- claim_closure(n_vector, claim_sizes, setldel_mean, setldel_cv)

###############################################################################
##                       5. Number of Partial payments                       ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
benchmark_1 <- 0.0375 * ref_claim
benchmark_2 <- 0.075 * ref_claim
simulate_no_pmt <- function(claim_size, claim_size_benchmark_1, claim_size_benchmark_2) {
  # WARNING: Do not change function arguments
  if (claim_size <= claim_size_benchmark_1) {
    return(sample(c(1, 2), size = 1, replace = TRUE, prob = c(1/2, 1/2)))
  } else if (claim_size_benchmark_1 < claim_size & claim_size <= claim_size_benchmark_2) {
    return(sample(c(2, 3), size = 1, replace = TRUE, prob = c(1/3, 2/3)))
  } else {
    # It can be shown that E(M) = 3 + 1/p in this case
    no_pmt_mean <- min(8, 4 + log(claim_size/claim_size_benchmark_2))
    prob <- 1/(no_pmt_mean - 3)
    return(stats::rgeom(1, prob) + 4)
  }
}
## ----------------------------------------------------------------------------

# Number of partial payments
no_payments <- claim_payment_no(n_vector, claim_sizes, simulate_no_pmt,
                                claim_size_benchmark_1 = benchmark_1,
                                claim_size_benchmark_2 = benchmark_2)


###############################################################################
##            6. Sizes of Partial payments (constant dollar values)          ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
simulate_amt_pmt <- function(no_pmt, claim_size) {
  # WARNING: Do not change function arguments
  if (no_pmt >= 4) {
    ## 1) Simulate the "complement" of the proportion of total claim size represented by the last two payments
    p_mean <- 1 - min(0.95, 0.75 + 0.04*log(claim_size/(0.10 * ref_claim)))
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
    amt <- stats::rbeta(no_pmt - 2,
                        shape1 = p_unnorm_parameters[1], shape2 = p_unnorm_parameters[2])

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
## ----------------------------------------------------------------------------

# Sizes of partial payments
payment_sizes <- claim_payment_size(n_vector, claim_sizes, no_payments, simulate_amt_pmt)


###############################################################################
##                   7. Distribution of payments over time                   ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
simulate_d <- function(no_pmt, claim_size, setldel, occurrence_period,
                       setldel_mean_function) {
  # WARNING: Do not change function arguments
  result <- c(rep(NA, no_pmt))

  # First simulate the unnormalised values of d, sampled from a Weibull distribution
  if (no_pmt >= 4) {
    # 1) Simulate the last payment delay
    unnorm_d_mean <- (1 / 4) / time_unit
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
## ----------------------------------------------------------------------------

payment_delays <- claim_payment_delay(n_vector, claim_sizes, no_payments, setldel,
                                      setldel_mean, simulate_d)
# continuous time scale
payment_times <- claim_payment_time(n_vector, occurrence_times, notidel, payment_delays,
                                    discrete = FALSE)
# payment periods
payment_periods <- claim_payment_time(n_vector, occurrence_times, notidel, payment_delays,
                                      discrete = TRUE)


###############################################################################
##                             8. Claim inflation                            ##
###############################################################################

## ---------------------------- Input parameters ------------------------------
# Base inflation
# Annual inflation rate = 2% with quarterly compounding
demo_rate <- (1 + 0.02)^(1/4) - 1
base_inflation_past <- rep(demo_rate, times = 40)
base_inflation_future <- rep(demo_rate, times = 40)
base_inflation_vector <- c(base_inflation_past, base_inflation_future)

# Superimposed inflation:
# 1) With respect to occurrence "time" (continuous scale)
super_infl_i <- function(occurrence_time, claim_size) {
  if (occurrence_time <= 20 / 4 / time_unit) {1}
  else {1 - 0.4*max(0, 1 - claim_size/(0.25 * ref_claim))}
}
# 2) With respect to payment "time" (continuous scale)
# -> compounding by user-defined time unit
super_infl_t <- function(payment_time, claim_size) {
  period_rate <- (1 + 0.30)^(time_unit) - 1
  beta <- period_rate * max(0, 1 - claim_size/ref_claim)
  (1 + beta)^payment_time
}
## ----------------------------------------------------------------------------

payment_inflated <- claim_payment_inflation(
  n_vector,
  payment_sizes,
  payment_times,
  occurrence_times,
  claim_sizes,
  base_inflation_vector,
  super_infl_i,
  super_infl_t
)

###############################################################################
##                       Creation of External Data                           ##
###############################################################################
## test_claims_object
test_claims_object <- claims(
  n_vector, occurrence_times, claim_sizes, notidel, setldel, no_payments,
  payment_sizes, payment_delays, payment_times, payment_inflated)
usethis::use_data(test_claims_object, overwrite = TRUE)

## test_claim_dataset
test_claim_dataset <- generate_claim_dataset(
  frequency_vector = n_vector,
  occurrence_list = occurrence_times,
  claim_size_list = claim_sizes,
  notification_list = notidel,
  settlement_list = setldel,
  no_payments_list = no_payments
)
usethis::use_data(test_claim_dataset, overwrite = TRUE)

## test_transaction_dataset
test_transaction_dataset <- generate_transaction_dataset(
  test_claims_object,
  adjust = FALSE # to keep the original simulated payment times
)
usethis::use_data(test_transaction_dataset, overwrite = TRUE)
