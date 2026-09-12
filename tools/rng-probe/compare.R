## Compare probe-*.rds from the three runners. THROWAWAY DIAGNOSTIC.
##
## Reports, per stage, whether the platforms agree bit-for-bit; and where they
## do not, whether the disagreement is ULP-scale noise or a stream desync.

files  <- sort(list.files(".", pattern = "^probe-.*\\.rds$", recursive = TRUE,
                          full.names = TRUE))
probes <- lapply(files, readRDS)
names(probes) <- vapply(probes, function(p) p$os, character(1))
stopifnot(length(probes) >= 2L)

ref_name <- if ("Darwin" %in% names(probes)) "Darwin" else names(probes)[1]
ref      <- probes[[ref_name]]
others   <- probes[setdiff(names(probes), ref_name)]

cat("Reference platform:", ref_name, "\n\n")
for (p in probes) {
  cat(sprintf("  %-8s %-28s SynthETIC %s | RNG %s\n",
              p$os, p$r_version, p$package_version,
              paste(p$rng_kind, collapse = "/")))
}

hdr <- function(s) cat("\n\n===", s, "===\n\n")

## --- Q1 -------------------------------------------------------------------
hdr("Q1. Do R's own functions return identical values across platforms?")
mnames <- names(ref$micro_fp)
res <- data.frame(fun = mnames, stringsAsFactors = FALSE)
for (nm in names(others)) {
  o <- others[[nm]]
  res[[nm]] <- ifelse(o$micro_fp[mnames] == ref$micro_fp[mnames], "same", "DIFFERS")
  res[[paste0(nm, "_n_unif")]] <- ifelse(
    o$micro_consumed[mnames] == ref$micro_consumed[mnames],
    ref$micro_consumed[mnames],
    sprintf("%s vs %s", o$micro_consumed[mnames], ref$micro_consumed[mnames])
  )
}
print(res, row.names = FALSE)

## --- Q2/Q3 ----------------------------------------------------------------
hdr("Q2/Q3. Pipeline: where does it diverge, and is it noise or a desync?")
snames <- names(ref$stage_fp)
for (nm in names(others)) {
  o <- others[[nm]]
  cat("--", nm, "vs", ref_name, "--\n")
  rows <- lapply(snames, function(s) {
    a <- o$stage_values[[s]]; b <- ref$stage_values[[s]]
    if (length(a) != length(b)) {
      return(data.frame(stage = s, verdict = "DESYNC (length)",
                        detail = sprintf("%d vs %d", length(a), length(b))))
    }
    if (identical(o$stage_fp[[s]], ref$stage_fp[[s]])) {
      return(data.frame(stage = s, verdict = "identical", detail = ""))
    }
    d       <- which(a != b)
    reldiff <- abs(a[d] - b[d]) / pmax(abs(b[d]), .Machine$double.xmin)
    ulp     <- max(reldiff) < 1e-12
    data.frame(
      stage   = s,
      verdict = if (ulp) "ULP noise" else "DESYNC (values)",
      detail  = sprintf("%d/%d differ, max reldiff %.3g, sum %s",
                        length(d), length(a), max(reldiff),
                        if (isTRUE(all.equal(sum(a), sum(b), tolerance = 1e-12)))
                          "preserved" else "MOVED")
    )
  })
  print(do.call(rbind, rows), row.names = FALSE)
  cat("\n")
}

hdr("Verdict")
any_desync <- FALSE
for (nm in names(others)) {
  o <- others[[nm]]
  for (s in snames) {
    a <- o$stage_values[[s]]; b <- ref$stage_values[[s]]
    if (length(a) != length(b)) { any_desync <- TRUE; break }
    d <- which(a != b)
    if (length(d) &&
        max(abs(a[d] - b[d]) / pmax(abs(b[d]), .Machine$double.xmin)) >= 1e-12) {
      any_desync <- TRUE; break
    }
  }
}
cat(if (any_desync) {
  "A genuine RNG-stream desync is present. The variable-consumption samplers\nare firing, and PR #6's diagnosis holds.\n"
} else if (all(vapply(others, function(o) identical(o$stage_fp, ref$stage_fp), logical(1)))) {
  "No cross-platform difference at all. Nothing to fix in the samplers.\n"
} else {
  "Differences are ULP-scale only, with lengths and totals intact. No stream\ndesync: the rejection loop and other amplifiers are not firing here.\n"
})
