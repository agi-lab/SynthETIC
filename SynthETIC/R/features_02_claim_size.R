###############################################################################
##                   2. Claim size (constant dollar values)                  ##
###############################################################################

#' Claim Size
#'
#' Simulates and returns the size of each of the claims occurring in each of
#' the periods, given its cumulative distribution function.
#' \cr \cr Note that \code{claim_size()} aims to model the claim sizes
#' **without inflation**.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param simfun optional alternative sampling distribution; see Details.
#' @param type the type of `simfun` provided. The default is a random generation
#' function (e.g. `rweibull`); the alternative `"p"` is any valid cumulative
#' distribution function (e.g. `pweibull`).
#' @param ... other arguments/parameters to be passed onto \code{simfun}.
#' @return A list of claim sizes such that the \eqn{i}th component of the
#' list gives the sizes for all claims that occurred in period \eqn{i}.
#' @details By default \code{claim_size()} assumes a left truncated power
#' normal distribution: \eqn{S^0.2 ~ Normal (9.5, sd = 3)}, left truncated
#' at 30. The truncation is done via resampling for rejected values. \cr \cr
#' Users can opt to use alternative distributions if desired. As discussed in
#' \code{\link{claim_frequency}}, users can declare such specification through
#' the `simfun` argument, which takes both random generation functions
#' (`type = "r"`, the default) and cumulative distribution functions
#' (`type = "p"`). See Examples. \cr \cr
#' For the latter, `claim_size()` will first search for the existence of the
#' corresponding `r`-function. If it notes the existence of such an `r`-function
#' (e.g. `rweibull` for `simfun = pweibull`), it will directly apply the
#' `r`-function to optimise simulation efficiency. Otherwise, the function uses
#' a numerical inverse transform method for simulation (see
#' \code{\link{simulate_cdf}}), which may not be the most efficient and can
#' potentially result in errors if an appropriate `range` is not specified in
#' the optional arguments.
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#' claim_size(n_vector)[[1]] # gives the sizes for all
#'                           # all claims incurred in period 1
#'
#' # use some custom pre-defined distribution function
#' claim_size(n_vector, stats::rweibull, shape = 4, scale = 100000)[[1]]
#' # equivalently
#' claim_size(n_vector, stats::pweibull, "p", shape = 4, scale = 100000)[[1]]
#' @export
claim_size <- function(frequency_vector, simfun, type = c("r", "p"), ...) {
  I <- length(frequency_vector)
  total_claim <- sum(frequency_vector)
  claim_sizes <- vector("list", I)

  if (!missing(simfun)) {
    type <- match.arg(type)
    if (type == "r") {
      for (i in 1:I) {
        claim_sizes[[i]] <- simfun(frequency_vector[i], ...)
      }
    } else if (type == "p") {
      ## first search for whether there is a corresponding r-function
      ## if yes, call the r-function
      ## else, use numerical methods to inverse transform (simulate_cdf)

      # get function name
      name_as_chr <- deparse(substitute(simfun))
      # split input by :: if simfun is input in the form of "foo::bar"
      name_contains_pkg <- grepl("::", name_as_chr)
      if (name_contains_pkg == TRUE) {
        pkg <- unlist(strsplit(name_as_chr, split = "::"))[1]
        fun <- unlist(strsplit(name_as_chr, split = "::"))[2]
        # does an r-function exist?
        substr(fun, 1, 1) <- "r"
        found_r <- exists(fun, where = asNamespace(pkg), mode = "function")
      } else {
        fun <- name_as_chr
        # does an r-function exist?
        substr(fun, 1, 1) <- "r"
        found_r <- exists(fun, mode = "function")
      }

      if (found_r == TRUE) {
        rfun <- ifelse(exists("pkg"), getExportedValue(pkg, fun), get(fun))
        for (i in 1:I) {
          claim_sizes[[i]] <- rfun(frequency_vector[i], ...)
        }
      } else {
        # if there is no such r-function, then use the distribution function
        # to inverse transform
        for (i in 1:I) {
          claim_sizes[[i]] <- simulate_cdf(frequency_vector[i], simfun,  ...)
        }
      }
    }
  } else {
    # if no distribution function is provided, use default power normal
    s <- (stats::rnorm(total_claim, mean = 9.5, sd = 3))^5
    while (any(s < 30)) {
      for (j in which(s < 30)) {
        s[j] <- (stats::rnorm(1, mean = 9.5, sd = 3))^5
      }
    }
    # re-scale the claim sizes according to ref_claim value provided
    s <- .pkgenv$ref_claim / 200000 * s
    claim_sizes <- to_SynthETIC(s, frequency_vector)
  }

  claim_sizes
}
