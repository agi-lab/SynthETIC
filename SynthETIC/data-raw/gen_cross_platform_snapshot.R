## Canonical macOS snapshot for cross-platform reproducibility regression test.
##
## Run this script on the canonical platform (currently macOS, R release) and
## commit the resulting `tests/testthat/cross_platform_snapshot.rda`. The
## testthat test `test-cross-platform-reproducibility.R` then re-runs the same
## pipeline on each CI platform (ubuntu / macos / windows) and asserts that
## the per-OS output still matches this snapshot.
##
## Regenerate the snapshot whenever the *intended* numeric output of the
## simulator changes (e.g. after fixing an OS-dependent sampler). Do NOT
## regenerate it merely to silence a Windows/Linux test failure unless that
## failure is the expected consequence of an intentional change.

suppressWarnings({
  library(SynthETIC)
})

SEED <- 20260712
I <- 40L
LAMBDA <- 100

snapshot <- new.env(parent = emptyenv())

snapshot$seed <- SEED
snapshot$n_periods <- I
snapshot$lambda <- LAMBDA
snapshot$r_version <- R.version.string
snapshot$rng_kind <- RNGkind()
snapshot$os <- Sys.info()[["sysname"]]
snapshot$os_version <- paste(Sys.info()[["release"]], Sys.info()[["machine"]])
snapshot$package_version <- as.character(packageVersion("SynthETIC"))

# Use the canonical RNG configuration (R defaults). If the defaults change in
# a future R release the cross-platform comparison would change meaning, so
# the test will guard on these fields and skip rather than silently fail.
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

set.seed(SEED)

# M1 -- claim frequency (single rpois draw per period; OS-stable per the
# reproducibility investigation, included as a control)
freq <- claim_frequency(I = I, simfun = stats::rpois, lambda = LAMBDA)
snapshot$freq <- freq

# M1 -- occurrence time within each period (runif inversion; OS-stable)
occurrence_times <- claim_occurrence(freq)
snapshot$occurrence_times <- occurrence_times

# M2 -- claim size (default power-normal with rejection-based truncation;
# THIS is the first OS-divergence point reported in
# CROSS_PLATFORM_REPRODUCIBILITY.md)
claim_sizes <- claim_size(freq)
snapshot$claim_sizes <- claim_sizes

# M2a -- covariate simulation via rmultinom. The categorical draws are
# mathematically independent of the claim sizes *values*, but they consume
# uniforms from the same stream: any stream shift upstream (e.g. an extra
# rejection draw in M2) reshuffles the categorical assignments downstream.
adj <- claim_size_adj(test_covariates_obj, claim_sizes)
snapshot$covariates_data <- adj$covariates_data
snapshot$claim_sizes_adj <- adj$claim_size_adj

# M5 -- number of partial payments. Default uses sample() + rgeom (categorical
# OS-deterministic given fixed uniform position, therefore sensitive to any
# upstream stream shift).
no_payments <- claim_payment_no(freq, adj$claim_size_adj)
snapshot$no_payments <- no_payments

# M6 -- payment sizes. Default uses stats::rbeta, a rejection sampler; this is
# the second OS-divergence point and the target of Layer B in the fix plan.
payment_sizes <- claim_payment_size(freq, adj$claim_size_adj, no_payments)
snapshot$payment_sizes <- payment_sizes

# Persist as a single rda.
out_path <- file.path("tests", "testthat", "cross_platform_snapshot.rds")
saveRDS(snapshot, out_path)
cat("Snapshot written to:", out_path, "\n")
cat("freq vector:", paste(freq, collapse = " "), "\n")
cat("total claims:", sum(freq), "\n")
cat("total payments:", sum(unlist(no_payments)), "\n")