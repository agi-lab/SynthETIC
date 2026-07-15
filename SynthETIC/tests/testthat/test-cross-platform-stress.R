# Cross-platform reproducibility stress test (~40k claims, 10x scale)
# ----------------------------------------------------------------------------
# Same pipeline as test-cross-platform-reproducibility.R but at 10x scale
# (lambda = 1000 -> ~40,000 claims, ~200,000 payments). At high N a 1-ULP
# libm difference is more likely to land a uniform on a findInterval boundary
# and flip a single categorical draw; this test verifies that the Layer A/A2
# fixes (pure-inversion samplers with fixed uniform counts) hold up at scale
# and that no residual OS-dependent path amplifies into a stream desync.
#
# Regenerate the snapshot by running `data-raw/gen_snapshots.R` on the
# canonical platform (macOS) alongside the standard snapshot.

SNAPSHOT_PATH <- test_path("cross_platform_stress_snapshot.rds")
snapshot <- readRDS(SNAPSHOT_PATH)

test_that("stress snapshot RNG configuration is active", {
  check_rng_config(snapshot)
})

test_that("stress-scale pipeline matches macOS snapshot on all OSes", {
  out <- run_from_snapshot(snapshot)
  assert_cross_platform(out, snapshot)
})