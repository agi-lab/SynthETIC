## Multi-seed cross-platform RNG probe -- THROWAWAY DIAGNOSTIC.
##
## The single-seed probe showed ULP-scale differences with no stream desync.
## A desync is a rare event by nature: it needs a rejection comparison to land
## within one ULP of its boundary. One clean seed therefore shows only that it
## did not fire for that seed.
##
## This runs N seeds and records a compact fingerprint per stage, so the three
## platforms can be compared without shipping hundreds of megabytes of raw
## deviates. Per seed and stage we keep:
##
##   len  -- a change here IS a desync: the stream consumed a different number
##           of uniforms and the claim or payment count moved
##   sum  -- stored as a double, which RDS preserves exactly. Under ULP noise
##           the sum is preserved to ~1e-14; under a desync the values are
##           independent draws and the sum moves at CLT scale, orders of
##           magnitude larger
##   fp   -- digest of the exact IEEE hex form of every value, so bit-identity
##           can be tested without keeping the values
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

run_pipeline <- function(seed) {
  set.seed(seed)
  n_vector         <- claim_frequency(I = 40, E = 12000, lambda = 0.03)
  occurrence_times <- claim_occurrence(n_vector)
  claim_sizes      <- claim_size(n_vector)      # default power-normal branch
  notidel          <- claim_notification(n_vector, claim_sizes)
  setldel          <- claim_closure(n_vector, claim_sizes)
  no_payments      <- claim_payment_no(n_vector, claim_sizes)
  payment_sizes    <- claim_payment_size(n_vector, claim_sizes, no_payments)
  payment_delays   <- claim_payment_delay(n_vector, claim_sizes, no_payments,
                                          setldel)
  list(n_vector = n_vector, occurrence_times = occurrence_times,
       claim_sizes = claim_sizes, notidel = notidel, setldel = setldel,
       no_payments = no_payments, payment_sizes = payment_sizes,
       payment_delays = payment_delays)
}

## Digest the hex form rather than the doubles: the %a representation is
## identical on every platform for identical bits, so a fingerprint mismatch
## always means a real value difference and never a serialisation quirk.
fingerprint <- function(x) {
  x <- as.double(unlist(x, use.names = FALSE))
  list(len = length(x),
       sum = sum(x),
       fp  = digest::digest(sprintf("%a", x), algo = "md5"))
}

stage_names <- c("n_vector", "occurrence_times", "claim_sizes", "notidel",
                 "setldel", "no_payments", "payment_sizes", "payment_delays")

rows <- vector("list", length(seeds) * length(stage_names))
k <- 0L
t0 <- Sys.time()
for (i in seq_along(seeds)) {
  out <- run_pipeline(seeds[i])
  for (s in stage_names) {
    f <- fingerprint(out[[s]])
    k <- k + 1L
    rows[[k]] <- data.frame(seed = seeds[i], stage = s, len = f$len,
                            sum = f$sum, fp = f$fp, stringsAsFactors = FALSE)
  }
  if (i %% 50L == 0L) {
    cat(sprintf("  %d/%d seeds (%.1f min elapsed)\n", i, length(seeds),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    flush.console()
  }
}

result <- list(
  os        = Sys.info()[["sysname"]],
  r_version = R.version.string,
  seeds     = seeds,
  data      = do.call(rbind, rows)
)
out <- sprintf("multiseed-%s.rds", tolower(result$os))
saveRDS(result, out)
cat(sprintf("\n%s: %d seeds x %d stages -> %s\n",
            result$os, length(seeds), length(stage_names), out))
