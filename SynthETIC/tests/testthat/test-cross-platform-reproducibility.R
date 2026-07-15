# Cross-platform reproducibility regression test
# ----------------------------------------------------------------------------
# Reproduces the OS-dependence bug documented in `CROSS_PLATFORM_REPRODUCIBILITY.md`:
# with a fixed RNG seed, the default SynthETIC pipeline (freq -> occurrence ->
# claim_size -> covariates -> payment counts -> payment sizes) yields identical
# output on a given OS but diverges across OSes. The cause is samplers that
# consume a *variable* number of uniforms per deviate:
#
#   * `claim_size()` default power-normal: previously a `while (any(s < 30))`
#     rejection loop. A 1-ULP libm difference in the accept/reject comparison
#     consumes one extra uniform on one OS but not another, desyncing every
#     downstream module. FIXED (Layer A): pure inverse-transform now consumes
#     exactly `total_claim` uniforms.
#   * `simulate_covariates()` default categorical: previously `rmultinom(n, 1, p)`,
#     which internally walks each trial with successive rbinom draws and stops
#     early once the size is zero -- a per-trial variable uniform count. FIXED
#     (Layer A2): pure inversion via findInterval, exactly n uniforms.
#   * `claim_payment_size()` default payment proportions: still uses
#     `stats::rbeta`, a rejection sampler (variable uniform count). Not yet
#     fixed (planned Layer B); this test will surface it once the upstream
#     modules stop desyncing the stream.
#
# This test compares live output against a canonical macOS snapshot
# (`cross_platform_snapshot.rds`). On each OS the test reports a single
# one-line summary of where the pipeline first diverges from the snapshot, so
# CI log noise stays minimal.
#
# When the *intended* numeric output of the simulator changes (e.g. after
# applying a fix), regenerate the snapshot by running
# `data-raw/gen_cross_platform_snapshot.R` on the canonical platform and
# committing the updated `cross_platform_snapshot.rds`. Do NOT regenerate it
# merely to silence a platform failure unless that failure is the expected
# consequence of an intentional change.

SNAPSHOT_PATH <- test_path("cross_platform_snapshot.rds")
snapshot <- readRDS(SNAPSHOT_PATH)

# ---------------------------------------------------------------------------
# Concise comparison helpers. testthat's default expect_identical/expect_equal
# print every differing row/value, which produces megabytes of uninformative
# output when the cross-platform bug fires (thousands of categorical rows or
# numeric deviates differ only because one upstream draw flipped). These
# helpers reduce each assertion to at most one line of diagnostic text.
# ---------------------------------------------------------------------------

# Compare two numeric (atomic or list-of-atomics) objects by flattening,
# computing the count of contrasting elements, the first divergent index,
# the values at that index, and the maximum relative difference. Returns a
# one-line failure message describing the divergence, or NULL if identical
# within `tolerance`.
diff_numeric <- function(label, actual, expected,
                        tolerance = sqrt(.Machine$double.eps)) {
  a <- unlist(actual, use.names = FALSE)
  e <- unlist(expected, use.names = FALSE)
  if (length(a) != length(e)) {
    return(sprintf(
      "%s: length mismatch (actual=%d, expected=%d)",
      label, length(a), length(e)
    ))
  }
  na_mismatch <- (is.na(a) != is.na(e))
  rdiff <- abs(a - e) / pmax(abs(e), .Machine$double.xmin)
  bad <- which(na_mismatch | rdiff > tolerance)
  if (length(bad) == 0L) return(NULL)
  i <- bad[1L]
  sprintf(
    "%s: %d of %d deviate; first at idx %d (actual=%s, snap=%s, max_reldiff=%g)",
    label, length(bad), length(a), i,
    format(a[i], digits = 8), format(e[i], digits = 8),
    max(rdiff[bad])
  )
}

# Compare two data.frames (categorical covariates) and report the first row
# whose columns differ and how many rows total differ. Returns a one-line
# message or NULL if identical.
diff_categorical <- function(label, actual, expected) {
  if (!identical(dim(actual), dim(expected))) {
    return(sprintf(
      "%s: dim mismatch (actual=%s, expected=%s)",
      label, paste(dim(actual), collapse = "x"),
      paste(dim(expected), collapse = "x")
    ))
  }
  diff_rows <- which(rowSums(as.matrix(actual != expected)) > 0L)
  if (length(diff_rows) == 0L) return(NULL)
  i <- diff_rows[1L]
  sprintf(
    "%s: %d of %d rows differ; first at row %d (actual=[%s], snap=[%s])",
    label, length(diff_rows), nrow(actual), i,
    paste(actual[i, ], collapse = "/"),
    paste(expected[i, ], collapse = "/")
  )
}

# Wrapper: run `check` (returns NULL or one-line message), and assert with
# expect_true so testthat does not echo the entire data diff.
expect_no_diff <- function(msg) {
  expect_true(is.null(msg), info = msg %||% "")
}

# ---------------------------------------------------------------------------
# Guard against future changes to R's default RNG knobs -- if R ever changes
# the defaults, a cross-platform comparison against this snapshot would no
# longer be a like-for-like check, so skip rather than silently misreport.
# ---------------------------------------------------------------------------
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

# Recomputes the pipeline from a single seed. Defined here (instead of in a
# helper file) so any future change to the call sequence stays in exactly one
# place alongside the snapshot it validates.
run_pipeline <- function() {
  # Order matters: R's RNGkind() silently resets .Random.seed when the active
  # kind actually changes. Match the order used by the snapshot generator
  # (RNGkind first, then set.seed) so the two runs share the same stream.
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
  payment_sizes <- claim_payment_size(freq, adj$claim_size_adj, no_payments)

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

# M1 -- frequency + occurrence time. Inversion + rpois; OS-stable already per
# the reproducibility investigation. Sanity control: a failure here signals
# RNG/stream setup drift, not the libm bug under test.
test_that("M1 frequency and occurrence times are OS-stable (control)", {
  out <- run_pipeline()
  expect_no_diff(diff_numeric("M1 freq", out$freq, snapshot$freq))
  expect_no_diff(diff_numeric(
    "M1 occurrence_times", out$occurrence_times, snapshot$occurrence_times
  ))
})

# M2 -- claim size. First OS-divergence point prior to Layer A; now uses pure
# inverse transform (exactly one uniform per claim) and should match the
# snapshot within machine-epsilon on all OSes.
test_that("M2 claim sizes are cross-platform reproducible", {
  out <- run_pipeline()
  expect_no_diff(diff_numeric(
    "M2 claim_sizes", out$claim_sizes, snapshot$claim_sizes
  ))
})

# M2a -- covariate simulation. Previously used rmultinom (per-trial variable
# uniform count via early-stopping rbinom); now uses pure inversion
# (findInterval over cumsum(p), exactly n uniforms). The categorical
# assignments should be identical once the upstream RNG stream is stable.
test_that("M2a covariate assignments are cross-platform reproducible", {
  out <- run_pipeline()
  expect_no_diff(diff_categorical(
    "M2a covariates", out$covariates_data$data, snapshot$covariates_data$data
  ))
})

test_that("M2a covariate claim ids match the frequency vector", {
  out <- run_pipeline()
  expect_no_diff(diff_numeric(
    "M2a covariates ids", out$covariates_data$ids, snapshot$covariates_data$ids
  ))
})

# M2a (continued) -- adjusted claim sizes (after multiplying by severity
# relativities and renormalising). Same stream-sensitivity rationale as M2.
test_that("M2a adjusted claim sizes are cross-platform reproducible", {
  out <- run_pipeline()
  expect_no_diff(diff_numeric(
    "M2a claim_sizes_adj", out$claim_sizes_adj, snapshot$claim_sizes_adj
  ))
})

# M5 -- number of partial payments per claim. Integer-valued, downstream of
# M2a on the stream; expected to match exactly once M2/M2a stop desyncing.
test_that("M5 payment counts are cross-platform reproducible", {
  out <- run_pipeline()
  expect_no_diff(diff_numeric(
    "M5 no_payments", out$no_payments, snapshot$no_payments
  ))
})

# M6 -- payment sizes. Second OS-divergence point: stats::rbeta is a rejection
# sampler (variable uniform count). Not yet fixed; this test will stay red on
# Ubuntu/Windows (and may now surface as the first divergence point) until
# Layer B replaces rbeta with qbeta(runif(...)).
test_that("M6 payment sizes are cross-platform reproducible", {
  out <- run_pipeline()
  expect_no_diff(diff_numeric(
    "M6 payment_sizes", out$payment_sizes, snapshot$payment_sizes
  ))
})