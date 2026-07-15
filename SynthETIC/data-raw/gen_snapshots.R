## Snapshot generator for the cross-platform reproducibility regression tests.
##
## Regenerates the standard-scale snapshot (~4k claims) from the current
## SynthETIC source.
##
## Run from the SynthETIC package directory on the canonical platform
## (currently macOS, R release):
##
##     Rscript data-raw/gen_snapshots.R
##
## Commit the updated `tests/testthat/cross_platform_snapshot.rds`.
##
## Regenerate whenever the *intended* numeric output of the simulator changes
## (e.g. after fixing an OS-dependent sampler). Do NOT regenerate merely to
## silence a platform failure unless that failure is the expected consequence
## of an intentional change.

suppressWarnings({
  library(SynthETIC)
})

## Generate one snapshot from a (seed, n_periods, lambda) configuration.
gen_snapshot <- function(seed, n_periods, lambda) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)

  freq <- claim_frequency(I = n_periods, simfun = stats::rpois, lambda = lambda)
  occurrence_times <- claim_occurrence(freq)
  claim_sizes <- claim_size(freq)
  adj <- claim_size_adj(test_covariates_obj, claim_sizes)
  no_payments <- claim_payment_no(freq, adj$claim_size_adj)
  payment_sizes <- claim_payment_size(freq, adj$claim_size_adj, no_payments)

  snapshot <- list(
    seed              = seed,
    n_periods         = n_periods,
    lambda            = lambda,
    r_version         = R.version.string,
    rng_kind          = RNGkind(),
    os                = Sys.info()[["sysname"]],
    os_version        = paste(Sys.info()[["release"]], Sys.info()[["machine"]]),
    package_version   = as.character(packageVersion("SynthETIC")),
    freq              = freq,
    occurrence_times  = occurrence_times,
    claim_sizes       = claim_sizes,
    covariates_data   = adj$covariates_data,
    claim_sizes_adj   = adj$claim_size_adj,
    no_payments       = no_payments,
    payment_sizes     = payment_sizes
  )

  snapshot
}

## Standard-scale snapshot (~4,000 claims) -- mirrors the reproduction command
## from CROSS_PLATFORM_REPRODUCIBILITY.md.
standard <- gen_snapshot(seed = 20260712L, n_periods = 40L, lambda = 100)
out_standard <- file.path("tests", "testthat", "cross_platform_snapshot.rds")
saveRDS(standard, out_standard)
cat("Standard snapshot:", sum(standard$freq), "claims,",
    sum(unlist(standard$no_payments)), "payments ->", out_standard, "\n")