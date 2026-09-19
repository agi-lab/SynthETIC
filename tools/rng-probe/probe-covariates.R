## Multi-seed cross-platform probe of the COVARIATE pipeline -- THROWAWAY.
##
## The earlier probes ran the default pipeline without claim_size_adj(), so
## they never exercised simulate_covariates(). That path draws each claim's
## covariate combination with rmultinom(n, 1, relativities), which stops early
## per trial and so consumes a data-dependent number of uniforms: a flipped
## comparison there would desync everything downstream. This checks whether
## that ever happens across platforms.
##
## Two things are recorded:
##
##   relativities -- the freq and sev relativity vectors for every covariate
##                   combination, fingerprinted once. No RNG involved, so this
##                   isolates whether the arithmetic feeding rmultinom is
##                   itself platform-dependent.
##   data         -- per seed and stage: len, sum, digest (as in
##                   probe-multiseed.R), now including the drawn covariate
##                   combination and the adjusted claim sizes.
##
## sumsq and wsum (sum of i * x_i) are recorded alongside sum because several
## stages preserve their total by construction: claim_size_adj rescales to the
## unadjusted total, payment sizes sum to the claim size, and payment delays
## sum to the settlement delay. Comparing sums alone cannot see a desync there.
##
## Env: RNG_PROBE_SEEDS (default 500), RNG_PROBE_SEED0 (default 1).

suppressMessages({
  library(SynthETIC)
  library(digest)
})

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

n_seeds <- as.integer(Sys.getenv("RNG_PROBE_SEEDS", "500"))
seed0   <- as.integer(Sys.getenv("RNG_PROBE_SEED0", "1"))
seeds   <- seq.int(seed0, length.out = n_seeds)

hexfp <- function(x) digest::digest(sprintf("%a", as.double(x)), algo = "md5")

## ---- relativities, no RNG ------------------------------------------------
all_combinations <- expand.grid(test_covariates_obj$factors)
temp <- covariates_data(test_covariates_obj, data = all_combinations,
                        covariates_id = NULL)
relativities <- list(
  freq = covariates_relativity(temp, freq_sev = "freq"),
  sev  = covariates_relativity(temp, freq_sev = "sev")
)
relativity_fp <- vapply(relativities, hexfp, character(1))

## ---- pipeline --------------------------------------------------------------
## Encode each claim's drawn combination as one integer so it can be
## fingerprinted and summed like any other stage.
combo_code <- function(cov_data) {
  as.double(as.integer(interaction(cov_data$data, lex.order = TRUE,
                                   drop = FALSE)))
}

run_pipeline <- function(seed) {
  set.seed(seed)
  n_vector         <- claim_frequency(I = 40, E = 12000, lambda = 0.03)
  occurrence_times <- claim_occurrence(n_vector)
  claim_sizes      <- claim_size(n_vector)
  adj              <- claim_size_adj(test_covariates_obj, claim_sizes)
  claim_sizes_adj  <- adj$claim_size_adj
  notidel          <- claim_notification(n_vector, claim_sizes_adj)
  setldel          <- claim_closure(n_vector, claim_sizes_adj)
  no_payments      <- claim_payment_no(n_vector, claim_sizes_adj)
  payment_sizes    <- claim_payment_size(n_vector, claim_sizes_adj, no_payments)
  payment_delays   <- claim_payment_delay(n_vector, claim_sizes_adj,
                                          no_payments, setldel)
  list(n_vector = n_vector, occurrence_times = occurrence_times,
       claim_sizes = claim_sizes,
       covariate_combo = combo_code(adj$covariates_data),
       claim_sizes_adj = claim_sizes_adj,
       notidel = notidel, setldel = setldel, no_payments = no_payments,
       payment_sizes = payment_sizes, payment_delays = payment_delays)
}

fingerprint <- function(x) {
  x <- as.double(unlist(x, use.names = FALSE))
  list(len = length(x), sum = sum(x), sumsq = sum(x^2),
       wsum = sum(x * seq_along(x)), fp = hexfp(x))
}

rows <- list()
t0 <- Sys.time()
for (i in seq_along(seeds)) {
  out <- run_pipeline(seeds[i])
  for (s in names(out)) {
    f <- fingerprint(out[[s]])
    rows[[length(rows) + 1L]] <- data.frame(
      seed = seeds[i], stage = s, len = f$len, sum = f$sum,
      sumsq = f$sumsq, wsum = f$wsum, fp = f$fp, stringsAsFactors = FALSE)
  }
  if (i %% 50L == 0L) {
    cat(sprintf("  %d/%d seeds (%.1f min elapsed)\n", i, length(seeds),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    flush.console()
  }
}

result <- list(
  os            = Sys.info()[["sysname"]],
  r_version     = R.version.string,
  seeds         = seeds,
  relativity_fp = relativity_fp,
  data          = do.call(rbind, rows)
)
out <- sprintf("covseed-%s.rds", tolower(result$os))
saveRDS(result, out)
cat(sprintf("\n%s: %d seeds -> %s\n", result$os, length(seeds), out))
