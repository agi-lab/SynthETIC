# Cross-platform reproducibility regression test (standard scale, ~4k claims)
# ----------------------------------------------------------------------------
# Reproduces the OS-dependence bug documented in `CROSS_PLATFORM_REPRODUCIBILITY.md`:
# with a fixed RNG seed, the default SynthETIC pipeline (freq -> occurrence ->
# claim_size -> covariates -> payment counts -> payment sizes) yields identical
# output on a given OS but diverges across OSes. The cause was samplers that
# consume a *variable* number of uniforms per deviate:
#
#   * `claim_size()` default power-normal: previously a `while (any(s < 30))`
#     rejection loop. FIXED (Layer A): pure inverse-transform now consumes
#     exactly `total_claim` uniforms.
#   * `simulate_covariates()` default categorical: previously `rmultinom(n,1,p)`,
#     which internally walks each trial with successive rbinom draws and stops
#     early -- a per-trial variable uniform count. FIXED (Layer A2): pure
#     inversion via findInterval, exactly n uniforms.
#
# This test compares live output against a canonical macOS snapshot.
#
# When the *intended* numeric output of the simulator changes, regenerate the
# snapshot by running `data-raw/gen_snapshots.R` on the canonical
# platform and committing the updated `cross_platform_snapshot.rds`.

SNAPSHOT_PATH <- test_path("cross_platform_snapshot.rds")
snapshot <- readRDS(SNAPSHOT_PATH)

test_that("standard snapshot RNG configuration is active", {
  check_rng_config(snapshot)
})

test_that("standard-scale pipeline matches macOS snapshot on all OSes", {
  out <- run_from_snapshot(snapshot)
  assert_cross_platform(out, snapshot)
})