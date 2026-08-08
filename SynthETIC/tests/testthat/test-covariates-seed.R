# Regression tests for random number stream handling in simulate_covariates().
#
# Prior to v1.1.2, simulate_covariates() called set.seed(random_seed)
# unconditionally. With the default random_seed = NULL this became
# set.seed(NULL), which reseeds R's generator from system entropy, so the
# covariate draws (and every draw made afterwards) ignored the caller's seed
# and were not reproducible. When random_seed was supplied, the call left the
# global stream rewound to that seed, leaking the isolated seed into the
# surrounding simulation.
#
# These tests assert the *invariants* rather than pinned numeric values, so
# they stay valid across platforms and R releases.

freq <- c(5, 5)

sim_levels <- function(random_seed = NULL) {
    simulate_covariates(
        test_covariates_obj,
        frequency_vector = freq,
        random_seed = random_seed
    )$data
}

test_that("default random_seed = NULL respects the caller's seed", {
    set.seed(12345)
    first <- sim_levels()
    set.seed(12345)
    second <- sim_levels()

    expect_identical(first, second)
})

test_that("default random_seed = NULL does not reseed from entropy", {
    # Two different caller seeds must give different draws; if the function
    # reseeded itself from system entropy the caller's seed would be ignored.
    set.seed(1)
    a <- sim_levels()
    set.seed(2)
    b <- sim_levels()

    expect_false(identical(a, b))
})

test_that("supplied random_seed is reproducible", {
    set.seed(999)
    first <- sim_levels(random_seed = 42)
    # Deliberately advance the caller's stream between the two calls: the
    # result must depend only on random_seed.
    runif(10)
    second <- sim_levels(random_seed = 42)

    expect_identical(first, second)
})

test_that("supplied random_seed leaves the caller's stream undisturbed", {
    set.seed(777)
    before <- .GlobalEnv$.Random.seed

    invisible(sim_levels(random_seed = 42))

    # Compared via identical() rather than expect_identical() so that a failure
    # reports a single boolean instead of diffing a 626-element integer vector.
    expect_true(identical(.GlobalEnv$.Random.seed, before))
})

test_that("supplied random_seed does not create .Random.seed in a fresh session", {
    # Emulate a session in which no random number has yet been drawn.
    had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
    if (had_seed) {
        saved <- .GlobalEnv$.Random.seed
        on.exit(assign(".Random.seed", saved, envir = .GlobalEnv), add = TRUE)
        rm(list = ".Random.seed", envir = .GlobalEnv)
    } else {
        on.exit(
            suppressWarnings(rm(list = ".Random.seed", envir = .GlobalEnv)),
            add = TRUE
        )
    }

    invisible(sim_levels(random_seed = 42))

    expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
})

test_that("claim_size_adj propagates the same stream guarantees", {
    sizes <- list(c(1000, 2000, 3000), c(4000, 5000))

    set.seed(20200131)
    first <- claim_size_adj(test_covariates_obj, sizes)$claim_size_adj
    set.seed(20200131)
    second <- claim_size_adj(test_covariates_obj, sizes)$claim_size_adj
    expect_identical(first, second)

    set.seed(555)
    before <- .GlobalEnv$.Random.seed
    invisible(claim_size_adj(test_covariates_obj, sizes, random_seed = 7))
    expect_true(identical(.GlobalEnv$.Random.seed, before))
})
