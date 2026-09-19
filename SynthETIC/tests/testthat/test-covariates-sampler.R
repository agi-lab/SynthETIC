# Tests for the covariate category sampler in simulate_covariates().
#
# Up to v1.1.2 each claim's covariate combination was drawn with
# rmultinom(n, 1, prob). Its C implementation walks the categories with
# successive rbinom() calls and stops early, so the number of uniforms it
# consumes depends on the data; and in R 4.6 it accumulates the running
# probability total in long double, which is 80-bit on x86_64 but 64-bit on
# arm64. The two architectures therefore consumed different numbers of
# uniforms and every later draw in the simulation diverged.
#
# From v1.2.0 each claim consumes exactly one uniform, mapped through the
# cumulative relativities. As in test-covariates-seed.R, these tests assert
# invariants rather than pinned values.

combos <- expand.grid(test_covariates_obj$factors)
freq_relativity <- covariates_relativity(
    covariates_data(test_covariates_obj, data = combos, covariates_id = NULL),
    freq_sev = "freq"
)
combo_key <- function(df) do.call(paste, c(lapply(df, as.character), sep = "|"))

test_that("each claim consumes exactly one uniform", {
    n <- 500
    set.seed(2024)
    invisible(simulate_covariates(test_covariates_obj, frequency_vector = n))
    after_sampler <- .GlobalEnv$.Random.seed

    set.seed(2024)
    invisible(stats::runif(n))
    after_n_uniforms <- .GlobalEnv$.Random.seed

    expect_true(identical(after_sampler, after_n_uniforms))
})

test_that("combinations with zero relativity are never drawn", {
    zero_keys <- combo_key(combos[freq_relativity == 0, , drop = FALSE])
    expect_gt(length(zero_keys), 0) # the fixture must actually exercise this

    set.seed(99)
    drawn <- simulate_covariates(test_covariates_obj, frequency_vector = 5000)$data

    expect_false(any(combo_key(drawn) %in% zero_keys))
})

test_that("drawn combinations follow the normalised relativities", {
    n <- 20000
    set.seed(31)
    drawn <- simulate_covariates(test_covariates_obj, frequency_vector = n)$data

    p <- freq_relativity / sum(freq_relativity)
    observed <- as.numeric(table(factor(combo_key(drawn), levels = combo_key(combos)))) / n

    # Five binomial standard errors per combination, a loose bound that a
    # correct sampler passes and a mis-mapped one (e.g. off-by-one) fails.
    tolerance <- 5 * sqrt(p * (1 - p) / n) + 1e-12
    expect_true(all(abs(observed - p) <= tolerance))
})
