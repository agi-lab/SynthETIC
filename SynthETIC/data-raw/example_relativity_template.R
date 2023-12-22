factors <- list(
    "Legal Representation" = c("Y", "N"),
    "Injury Severity" = as.character(1:6),
    "Age of Claimant" = c("0-15", "15-30", "30-50", "50-65", "over 65")
)

relativity_freq <- relativity_template(factors)
relativity_sev <- relativity_template(factors)

# Default Values
relativity_freq$relativity <- c(
    1, 1,
    0.95, 1, 1, 1, 1, 1,
    0.05, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    0.53, 0.3, 0.1, 0.05, 0.01, 0.01,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    0.183, 0.192, 0.274, 0.18, 0.171
)

relativity_sev$relativity <- c(
    2, 1,
    1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    0.6, 1.2, 2.5, 5, 8, 0.4,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1, 0.97, 0.95,
    1, 1, 1, 0.95, 0.9,
    1, 1, 1, 1, 1,
    1.25, 1.15, 1, 0.85, 0.7
)

head(relativity_freq)
head(relativity_sev)

covariates_obj <- covariates(factors)
covariates_obj <- set.covariates_relativity(
    covariates = covariates_obj,
    relativity = relativity_freq,
    freq_sev = "freq"
)
covariates_obj <- set.covariates_relativity(
    covariates = covariates_obj,
    relativity = relativity_sev,
    freq_sev = "sev"
)

usethis::use_data(covariates_obj, overwrite = TRUE)
