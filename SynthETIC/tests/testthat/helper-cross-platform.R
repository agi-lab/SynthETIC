# Shared helpers for cross-platform reproducibility tests.
# testthat auto-sources files named helper-*.R before running any test.

# ---------------------------------------------------------------------------
# Concise comparison helpers. testthat's default expect_identical/expect_equal
# print every differing row/value, which produces megabytes of uninformative
# output when the cross-platform bug fires (thousands of categorical rows or
# numeric deviates differ only because one upstream draw flipped). These
# helpers reduce each assertion to at most one line of diagnostic text.
# ---------------------------------------------------------------------------

# Compare two numeric (atomic or list-of-atomics) objects by flattening and
# computing the count of differing elements, the first divergent index, and
# the maximum relative difference. Returns a one-line failure message or NULL
# if identical within `tolerance`.
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

# Wrapper: assert via expect_true so testthat does not echo the entire data
# diff. The one-line summary goes into `info`.
expect_no_diff <- function(msg) {
  expect_true(is.null(msg), info = msg %||% "")
}

# ---------------------------------------------------------------------------
# Pipeline runner. Mirrors gen_snapshots.R exactly so the live test and the
# snapshot generator always stay in lock-step. Parameterised by the snapshot's
# provenance fields so the test and generator share this single implementation.
# ---------------------------------------------------------------------------
run_cross_platform_pipeline <- function(seed, n_periods, lambda) {
  # Order matters: R's RNGkind() silently resets .Random.seed when the active
  # kind actually changes. Match the order used by the snapshot generators
  # (RNGkind first, then set.seed) so the two runs share the same stream.
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)

  freq <- claim_frequency(I = n_periods, simfun = stats::rpois, lambda = lambda)
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

# Run the pipeline using provenance stored in a snapshot object (an .rds
# produced by gen_snapshots.R).
run_from_snapshot <- function(snapshot) {
  run_cross_platform_pipeline(
    seed = snapshot$seed,
    n_periods = snapshot$n_periods,
    lambda = snapshot$lambda
  )
}

# Run all standard cross-platform assertions of `out` against `snapshot`.
# Each assertion produces at most one line of diagnostic text on failure.
assert_cross_platform <- function(out, snapshot) {
  expect_no_diff(diff_numeric("M1 freq", out$freq, snapshot$freq))
  expect_no_diff(diff_numeric(
    "M1 occurrence_times", out$occurrence_times, snapshot$occurrence_times
  ))
  expect_no_diff(diff_numeric("M2 claim_sizes", out$claim_sizes, snapshot$claim_sizes))
  expect_no_diff(diff_categorical(
    "M2a covariates", out$covariates_data$data, snapshot$covariates_data$data
  ))
  expect_no_diff(diff_numeric(
    "M2a covariates ids", out$covariates_data$ids, snapshot$covariates_data$ids
  ))
  expect_no_diff(diff_numeric(
    "M2a claim_sizes_adj", out$claim_sizes_adj, snapshot$claim_sizes_adj
  ))
  expect_no_diff(diff_numeric("M5 no_payments", out$no_payments, snapshot$no_payments))
  expect_no_diff(diff_numeric("M6 payment_sizes", out$payment_sizes, snapshot$payment_sizes))
}

# Guard: skip the calling test if the active RNG knobs don't match the
# snapshot's, so future R default changes don't silently misreport.
check_rng_config <- function(snapshot) {
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
}