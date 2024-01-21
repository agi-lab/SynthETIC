## code to prepare the test datasets with covariates.
## note that this code is identical to the test_data.R with the exception of
## applying claim size adjustments to covariates
## see the vignette for detailed comments on this code

## generated with default assumptions
set.seed(20200131)

###############################################################################
##                             1. Claim occurrence                           ##
###############################################################################
# Number of claims ocurring for each period i
n_vector <- claim_frequency(I = 40, E = 12000, lambda = 0.03)

# Occurrence time of each claim r, for each period i
occurrence_times <- claim_occurrence(n_vector)

###############################################################################
##                   2. Claim size (constant dollar values)                  ##
###############################################################################
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

# Apply Covariates
source("./data-raw/example_relativity_template.R")
usethis::use_data(test_covariates_obj, overwrite = TRUE)

test_claim_sizes_adj <- claim_size_adj(
    test_covariates_obj,
    claim_size(n_vector, S_df, type = "p", range = c(0, 1e24))
)
test_covariates_dataset <- test_claim_sizes_adj$covariates_data

usethis::use_data(test_covariates_dataset, overwrite = TRUE)

claim_sizes <- test_claim_sizes_adj$claim_size_adj

###############################################################################
##                           3. Claim notification                           ##
###############################################################################
notidel <- claim_notification(n_vector, claim_sizes)


###############################################################################
##                              4. Claim closure                             ##
###################################################
setldel <- claim_closure(n_vector, claim_sizes)


###############################################################################
##                       5. Number of Partial payments                       ##
###############################################################################
# Number of partial payments
no_payments <- claim_payment_no(n_vector, claim_sizes)


###############################################################################
##            6. Sizes of Partial payments (constant dollar values)          ##
###############################################################################
# Sizes of partial payments
payment_sizes <- claim_payment_size(n_vector, claim_sizes, no_payments)


###############################################################################
##                   7. Distribution of payments over time                   ##
###############################################################################
payment_delays <- claim_payment_delay(n_vector, claim_sizes, no_payments, setldel)

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
## ----------------------------------------------------------------------------

payment_inflated <- claim_payment_inflation(
    n_vector,
    payment_sizes,
    payment_times,
    occurrence_times,
    claim_sizes,
    base_inflation_vector
)

###############################################################################
##                       Creation of External Data                           ##
###############################################################################
## test_claims_object_cov
test_claims_object_cov <- claims(
    n_vector, occurrence_times, claim_sizes, notidel, setldel, no_payments,
    payment_sizes, payment_delays, payment_times, payment_inflated)
usethis::use_data(test_claims_object_cov, overwrite = TRUE)

## test_claim_dataset_cov
test_claim_dataset_cov <- generate_claim_dataset(
    frequency_vector = n_vector,
    occurrence_list = occurrence_times,
    claim_size_list = claim_sizes,
    notification_list = notidel,
    settlement_list = setldel,
    no_payments_list = no_payments
)
usethis::use_data(test_claim_dataset_cov, overwrite = TRUE)

## test_transaction_dataset_cov
test_transaction_dataset_cov <- generate_transaction_dataset(
    test_claims_object,
    adjust = FALSE # to keep the original simulated payment times
)
usethis::use_data(test_transaction_dataset_cov, overwrite = TRUE)
