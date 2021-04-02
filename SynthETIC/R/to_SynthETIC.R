#' Conversion to SynthETIC Format
#'
#' Converts a vector of simulated quantities (e.g. claim occurrence times, claim
#' sizes) to a list format consistent with what is used for `SynthETIC`
#' simulation; to be used when user wishes to replace one or more of the
#' `SynthETIC` modules with their own.
#'
#' @param x a vector of simulated quantities for all the claims.
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param level level of the data provided; one of `"clm"` (claim, which is the
#' default) or `"pmt"` (payment).
#' @param no_payments_list list of number of partial payments; only required
#' if `level = "pmt"`.
#' @details It is assumed that the simulated quantities in `x` is provided in
#' chronological order, e.g. if there are 30 claims in period 1 and `x` is
#' on a `"clm"` level, then the first 30 elements of `x` should give the
#' measures for those 30 claims. Likewise, if `x` is on a `"pmt"` level, and
#' the first claim in period 1 has 5 payments, then the first 5 elements of `x`
#' should give the measures for those 5 payments.
#' @return A list of quantities such that the \eqn{i}th component of the
#' list gives the corresponding measure for all claims that occurred in period
#' \eqn{i}.
#' @examples
#' freq <- claim_frequency()
#' my_claims <- rweibull(sum(freq), shape = 4, scale = 100000)
#' claim_sizes <- to_SynthETIC(my_claims, freq)
#' @export
to_SynthETIC <- function(x, frequency_vector, level = c("clm", "pmt"),
                         no_payments_list) {
  # default is claim
  level <- level[1]
  I <- length(frequency_vector)

  if (level == "clm") {
    if (length(x) != sum(frequency_vector)) {
      stop("The number of claims does not match.")
    }

    unname(split(x, rep(1:I, frequency_vector)))

  } else {
    # if level == "pmt"
    if (length(x) != sum(unlist(no_payments_list))) {
      stop("The number of payments does not match.")
    }

    # first split by occurrence period
    grouping <- sapply(no_payments_list, sum, simplify = TRUE)
    result <- split_tmp <- unname(split(x, rep(1:I, grouping)))
    # then split by claim
    for (i in 1:I) {
      J <- frequency_vector[i]
      result[[i]] <- unname(split(split_tmp[[i]], rep(1:J, no_payments_list[[i]])))
    }

    result

  }

}
