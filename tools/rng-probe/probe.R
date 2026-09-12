## Cross-platform RNG probe -- THROWAWAY DIAGNOSTIC, NOT PART OF THE PACKAGE.
##
## Answers three questions, on the current state of master:
##
##   Q1. Do R's own shipped random/quantile functions return bit-identical
##       values across Linux / macOS / Windows for a fixed seed?
##   Q2. Does the SynthETIC default pipeline diverge across platforms, and if
##       so at which stage?
##   Q3. If it diverges, is it ULP-scale noise (totals preserved, lengths
##       intact) or a genuine RNG-stream desync (row counts change)?
##
## Writes probe-<os>.rds; tools/rng-probe/compare.R diffs the three.

suppressMessages(library(SynthETIC))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

## Exact, lossless fingerprint of a double vector: %a is the IEEE hex form, so
## two fingerprints match if and only if every bit matches.
fingerprint <- function(x) {
  x <- unlist(x, use.names = FALSE)
  paste(sprintf("%a", as.double(x)), collapse = ",")
}

## ---------------------------------------------------------------------------
## Q1. Bare R functions. Each is seeded independently so that a divergence in
## one cannot mask or cause a divergence in another.
## ---------------------------------------------------------------------------
micro <- list(
  runif        = function() runif(1000),
  rnorm        = function() rnorm(1000, mean = 9.5, sd = 3),
  rnorm_pow5   = function() rnorm(1000, mean = 9.5, sd = 3)^5,
  rpois_small  = function() as.double(rpois(1000, lambda = 3)),
  rpois_large  = function() as.double(rpois(1000, lambda = 360)),
  rbeta        = function() rbeta(1000, shape1 = 2.5, shape2 = 6),
  rgeom        = function() as.double(rgeom(1000, prob = 0.4)),
  rmultinom    = function() as.double(rmultinom(200, 1, c(0.4, 0.3, 0.2, 0.1))),
  rweibull     = function() rweibull(1000, shape = 1.5, scale = 2),
  ## Quantile functions: no RNG, pure numerics. These isolate libm exposure
  ## (exp/log/pow) from anything to do with the random stream.
  qnorm_tail   = function() qnorm(c(1e-10, 1e-6, 0.0062, 0.5, 0.9938, 1 - 1e-6)),
  qnorm_grid   = function() qnorm(seq(1e-4, 1 - 1e-4, length.out = 1000), 9.5, 3),
  qbeta_grid   = function() qbeta(seq(1e-4, 1 - 1e-4, length.out = 1000), 2.5, 6),
  qpois_grid   = function() as.double(qpois(seq(1e-4, 1 - 1e-4, length.out = 1000), 360))
)

micro_fp <- vapply(names(micro), function(nm) {
  set.seed(20260912L)
  fingerprint(micro[[nm]]())
}, character(1))

## How many uniforms did each sampler actually consume? Fixed-consumption
## samplers give the same answer on every platform by construction; the point
## of recording it is to see whether a platform consumed a *different* number,
## which is the desync mechanism under investigation.
consumed <- function(f, max_draws = 12000L) {
  set.seed(1L); f(); target <- .Random.seed
  set.seed(1L); if (identical(.Random.seed, target)) return(0L)
  for (k in seq_len(max_draws)) {
    runif(1L)
    if (identical(.Random.seed, target)) return(k)
  }
  NA_integer_
}
micro_consumed <- vapply(names(micro), function(nm) consumed(micro[[nm]]), integer(1))

## ---------------------------------------------------------------------------
## Q2/Q3. The default SynthETIC pipeline, fingerprinted stage by stage.
## ---------------------------------------------------------------------------
set.seed(20200131L)
n_vector         <- claim_frequency(I = 40, E = 12000, lambda = 0.03)
occurrence_times <- claim_occurrence(n_vector)
claim_sizes      <- claim_size(n_vector)          # default power-normal branch
notidel          <- claim_notification(n_vector, claim_sizes)
setldel          <- claim_closure(n_vector, claim_sizes)
no_payments      <- claim_payment_no(n_vector, claim_sizes)
payment_sizes    <- claim_payment_size(n_vector, claim_sizes, no_payments)
payment_delays   <- claim_payment_delay(n_vector, claim_sizes, no_payments, setldel)

stages <- list(
  n_vector         = n_vector,
  occurrence_times = occurrence_times,
  claim_sizes      = claim_sizes,
  notidel          = notidel,
  setldel          = setldel,
  no_payments      = no_payments,
  payment_sizes    = payment_sizes,
  payment_delays   = payment_delays
)

## Keep the raw values too, so compare.R can tell ULP noise from a desync
## rather than only reporting "differs".
probe <- list(
  os              = Sys.info()[["sysname"]],
  os_detail       = paste(Sys.info()[["release"]], Sys.info()[["machine"]]),
  r_version       = R.version.string,
  package_version = as.character(packageVersion("SynthETIC")),
  rng_kind        = RNGkind(),
  micro_fp        = micro_fp,
  micro_consumed  = micro_consumed,
  stage_fp        = vapply(stages, fingerprint, character(1)),
  stage_len       = vapply(stages, function(x) length(unlist(x)), integer(1)),
  stage_sum       = vapply(stages, function(x) sum(unlist(x)), double(1)),
  stage_values    = lapply(stages, function(x) unlist(x, use.names = FALSE))
)

out <- sprintf("probe-%s.rds", tolower(probe$os))
saveRDS(probe, out)

cat("==== RNG probe:", probe$os, "|", probe$r_version, "====\n\n")
cat("-- bare R functions: uniforms consumed --\n")
print(probe$micro_consumed)
cat("\n-- pipeline stage lengths / sums --\n")
print(data.frame(length = probe$stage_len,
                 sum    = sprintf("%.17g", probe$stage_sum)))
cat("\nwrote", out, "\n")
