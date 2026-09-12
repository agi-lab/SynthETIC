## Compare multiseed-*.rds across platforms. THROWAWAY DIAGNOSTIC.
##
## For every (seed, stage), classify the platforms' agreement:
##
##   identical  -- bit-for-bit
##   ULP noise  -- values differ, length intact, sum preserved to 1e-10
##   DESYNC     -- length changed, or sum moved beyond 1e-10
##
## A single DESYNC anywhere means the variable-consumption samplers can in fact
## diverge across platforms, and PR #6's premise holds.

files <- list.files(".", pattern = "^multiseed-.*\\.rds$", recursive = TRUE,
                    full.names = TRUE)
probes <- lapply(files, readRDS)
names(probes) <- vapply(probes, function(p) p$os, character(1))
stopifnot(length(probes) >= 2L)

ref_name <- if ("Darwin" %in% names(probes)) "Darwin" else names(probes)[1]
ref <- probes[[ref_name]]
others <- probes[setdiff(names(probes), ref_name)]

cat("Reference platform:", ref_name, "\n")
for (p in probes) cat(sprintf("  %-8s %s | %d seeds\n", p$os, p$r_version,
                              length(p$seeds)))

classify <- function(a, b) {
  ## a, b are one-row-per-key data frames already aligned by (seed, stage)
  out <- rep("identical", nrow(a))
  diff_fp <- a$fp != b$fp
  out[diff_fp] <- "ULP noise"
  out[a$len != b$len] <- "DESYNC (length)"
  moved <- diff_fp & a$len == b$len &
    abs(a$sum - b$sum) / pmax(abs(b$sum), .Machine$double.xmin) > 1e-10
  out[moved] <- "DESYNC (sum)"
  out
}

any_desync <- FALSE
for (nm in names(others)) {
  o <- others[[nm]]
  a <- o$data; b <- ref$data
  key_a <- paste(a$seed, a$stage); key_b <- paste(b$seed, b$stage)
  common <- intersect(key_a, key_b)
  a <- a[match(common, key_a), ]; b <- b[match(common, key_b), ]

  verdict <- classify(a, b)
  cat("\n\n===", nm, "vs", ref_name, "-", length(unique(a$seed)), "seeds ===\n\n")
  print(table(stage = a$stage, verdict = verdict))

  bad <- grepl("^DESYNC", verdict)
  if (any(bad)) {
    any_desync <- TRUE
    cat("\n!! DESYNC cases (first 20):\n")
    print(head(data.frame(seed = a$seed[bad], stage = a$stage[bad],
                          verdict = verdict[bad],
                          len = paste(a$len[bad], "vs", b$len[bad])), 20),
          row.names = FALSE)
  }
}

cat("\n\n=== Verdict ===\n\n")
if (any_desync) {
  cat("At least one seed produced a cross-platform DESYNC. The\n",
      "variable-consumption samplers can diverge; PR #6's premise holds.\n", sep = "")
} else {
  cat(sprintf(
    "No desync in any of %d seeds on any platform. Every difference was\nULP-scale with lengths and sums intact.\n",
    length(ref$seeds)))
}
