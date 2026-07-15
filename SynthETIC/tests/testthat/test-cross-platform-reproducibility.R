# Cross-platform reproducibility regression test
# ----------------------------------------------------------------------------
# Reproduces the OS-dependence bug documented in
# `CROSS_PLATFORM_REPRODUCIBILITY.md`: with a fixed RNG seed, the default
# SynthETIC pipeline (freq -> occurrence -> claim_size -> covariates -> payment
# counts -> payment sizes) yields byte-identical output on a given OS but
# diverges across OSes because two samplers consume a variable number of
# uniforms:
#
#   * `claim_size()` default power-normal uses a `while (any(s < 30))` rejection
#     loop. A 1-ULP libm difference in the accept/reject comparison can flip a
#     single accept/reject decision, consuming one extra uniform on one OS but
#     not another. Every subsequent draw on the shared RNG stream then desyncs.
#   * `claim_payment_size()` default uses `stats::rbeta`, a rejection-based
#     gamma+rejection sampler. Same uniform-count-desync mechanism.
#
# This test compares the live pipeline output against a canonical snapshot
# (`cross_platform_snapshot.rds`) generated on the canonical platform (macOS).
# Pre-fix: the macOS runner reproduces the snapshot (green); ubuntu/windows
# runners diverge (red) -- reproducing the documented bug visible on CI.
# Post-fix (layer A + B replacing the variable-uniform-count samplers with
# pure inversion): all three runners reproduce the macOS snapshot to within
# machine-epsilon tolerance (green).
#
# When the *intended* numeric output of the simulator changes (e.g. when the
# fix is applied), regenerate the snapshot by running
# `data-raw/gen_cross_platform_snapshot.R` on the canonical platform and
# committing the updated `cross_platform_snapshot.rds`. Do NOT regenerate it
# just to silence a Windows/Linux test failure unless that failure is the
# expected consequence of an intentional change.

SNAPSHOT_PATH <- test_path("cross_platform_snapshot.rds")

snapshot <- readRDS(SNAPSHOT_PATH)

# Guard against future changes to R's default RNG knobs -- if R ever changes
# the defaults, a cross-platform comparison against this snapshot would no
# longer be a like-for-like check, so skip rather than silently misreport.
test_that("snapshot RNG configuration matches the active RNG configuration", {
  rng_now <- RNGkind()
  snap_rng <- snapshot$rng_kind
  if (!identical(rng_now, snap_rng)) {
    skip(paste0(
      "Active RNGkind (", paste(rng_now, collapse = "/"),
      ") does not match snapshot RNGkind (",
      paste(snap_rng, collapse = "/"),
      "). Regenerate the snapshot before relying on this test."
    ))
  }
  expect_identical(rng_now, snap_rng)
})

# Single-test helper that recomputes the pipeline from a single seed. We define
# it here (rather than in a helper) so any future change to the call sequence is
# mirrored in exactly one place alongside the snapshot it validates.
run_pipeline <- function() {
  # Order matters: R's RNGkind() silently resets .Random.seed when the active
  # kind actually changes. Match the order used by gen_cross_platform_snapshot.R
  # (RNGkind first, then set.seed) so the two run the same stream.
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(snapshot$seed)

  freq <- claim_frequency(
    I = snapshot$n_periods,
    simfun = stats::rpois,
    lambda = snapshot$lambda
  )
  occurrence_times <- claim_occurrence(freq)
  claim_sizes <- claim_size(freq)
  adj <- claim_size_adj(test_covariates_obj, claim_sizes)
  no_payments <- claim_payment_no(freq, adj$claim_size_adj)
  payment_sizes <- claim_payment_size(
    freq, adj$claim_size_adj, no_payments
  )

  list(
    freq = freq,
    occurrence_times = occurrence_times,
    claim_sizes = claim_sizes,
    covariates_data = adj$covariates_data,
    claim_sizes_adj = adj$claim_size_adj,
    no_payments = no_payments,
    payment_sizes = payment_sizes
  )
}

# M1 -- frequency + occurrence time. Inversion + rpois; expected to match the
# snapshot bit-for-bit on *every* OS today (already OS-stable per the
# reproducibility investigation), so a failure here signals RNG/stream setup
# drift rather than the libm bug under test.
test_that("M1 frequency and occurrence times are OS-stable (control)", {
  out <- run_pipeline()
  expect_identical(out$freq, snapshot$freq)
  expect_identical(out$occurrence_times, snapshot$occurrence_times)
})

# M2 -- claim size. First OS-divergence point. Pre-fix: red on ubuntu/windows
# (desync of the rejection loop); red on ubuntu/windows here while macOS stays
# green. Post-fix (layer A): green on all OSes within machine-epsilon tolerance.
test_that("M2 claim sizes are cross-platform reproducible", {
  out <- run_pipeline()
  # Use expect_equal (relative tolerance sqrt(.Machine$double.eps)) rather than
  # expect_identical: layer A fixes the *stream desync*, but per-claim draws go
  # through libm `qnorm` which can differ at ~1 ULP across platforms.
  expect_equal(out$claim_sizes, snapshot$claim_sizes,
               info = "claim_sizes diverged across OSes (claim_size rejection loop)")
})

# M2a -- covariate simulation via rmultinom. Categorical assignments are
# downstream of M2 on the shared RNG stream: a single shifted uniform in M2
# reshuffles every categorical draw here. expect_identical because the columns
# are strings/factors -- once the stream is stable, the assignment should be
# bit-identical across OSes (no libm in the categorical path).
test_that("M2a covariate assignments are cross-platform reproducible", {
  out <- run_pipeline()
  expect_identical(out$covariates_data, snapshot$covariates_data,
                   info = "covariates reshuffled by an upstream RNG stream shift")
})

# M2a (continued) -- adjusted claim sizes. Same stream-sensitivity rationale
# as M2; numerically they are multiplied by relativities as a final step.
test_that("M2a adjusted claim sizes are cross-platform reproducible", {
  out <- run_pipeline()
  expect_equal(out$claim_sizes_adj, snapshot$claim_sizes_adj,
               info = "claim_sizes_adj diverged across OSes")
})

# M5 -- number of partial payments per claim. Integer-valued, downstream of
# M2a on the stream. expect_identical because integers should match exactly
# once the stream is stable.
test_that("M5 payment counts are cross-platform reproducible", {
  out <- run_pipeline()
  expect_identical(out$no_payments, snapshot$no_payments,
                   info = "no_payments diverged across OSes")
})

# M6 -- payment sizes. Second OS-divergence point (rbeta rejection sampler).
# Pre-fix: red on ubuntu/windows; macOS green. Post-fix (layer B): green on all
# OSes within machine-epsilon.
test_that("M6 payment sizes are cross-platform reproducible", {
  out <- run_pipeline()
  expect_equal(out$payment_sizes, snapshot$payment_sizes,
               info = "payment_sizes diverged across OSes (rbeta rejection sampler)")
})