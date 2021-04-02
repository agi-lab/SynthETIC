###############################################################################
##                           1. Claim occurrence                             ##
###############################################################################

#' Claim Frequency
#'
#' Returns the number of insurance claims occurring in each of the periods.
#'
#' @param I number of claims development periods considered.
#' @param E **effective annual** exposure associated with each period (vector).
#' @param freq expected frequency per unit exposure for each period (vector).
#' @param simfun optional alternative sampling distribution; see Details.
#' @param type the type of `simfun` provided. The default is a random generation
#' function (e.g. `rpois`); the alternative `"p"` is any valid cumulative
#' distribution function (e.g. `ppois`).
#' @param ... other arguments/parameters to be passed onto \code{simfun}.
#' @details Unless otherwise specified, \code{claim_frequency()} assumes the
#' claim frequency follows a Poisson distribution with mean equal to the
#' product of exposure \code{E} associated with period \eqn{i} and expected
#' claim frequency \code{freq} per unit exposure for that period. \cr \cr
#' If no arguments are provided, by default \code{claim_frequency()} assumes
#' a total of 40 development periods, constant exposure rate at 12000 per year
#' and constant frequency at 0.03 per unit of exposure. \cr \cr
#' If one wishes to use an alternative sampling distribution for claim
#' frequency, they could declare such specification through the `simfun`
#' argument. The `simfun` argument takes both random generation functions
#' (`type = "r"`, the default) and cumulative distribution functions
#' (`type = "p"`). For the latter, `claim_frequency()` will first search for the
#' existence of the corresponding `r`-function. If it notes the existence of
#' such an `r`-function (e.g. `rpois` for `simfun = ppois`), it will directly
#' apply the `r`-function to optimise simulation efficiency. Otherwise, the
#' function uses a numerical inverse transform method for simulation (see
#' \code{\link{simulate_cdf}}), which may not be the most efficient and can
#' potentially result in errors if an appropriate `range` is not specified in
#' the optional arguments. \cr \cr
#' Pre-defined distribution functions such as \code{ppois} are supported.
#' @examples
#' no_period <- 40
#' exposure <- c(rep(12000, no_period))
#' exp_freq <- c(rep(0.03, no_period))
#' # returns the same result as claim_frequency()
#' claim_frequency(I = no_period, E = exposure, freq = exp_freq)
#'
#' # use a pre-defined random generation function
#' claim_frequency(I = 10, simfun = rpois, lambda = 80)
#' # or equivalently, through a distribution function
#' claim_frequency(I = 10, simfun = ppois, type = "p", lambda = 80)
#' @export
claim_frequency <- function(
  I = 40, E = 12000, freq = 0.03, simfun, type = c("r", "p"), ...) {
  if (!missing(simfun)) {
    type <- match.arg(type)
    if (type == "r") {
      simfun(I, ...)
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
        rfun(I, ...)
      } else {
        # if there is no such r-function, then use the distribution function
        # to inverse transform
        simulate_cdf(I, simfun, ...)
      }
    }
  } else {
    # if no custom simfun is defined, assume Poisson distribution by default
    stats::rpois(I, E * .pkgenv$time_unit * freq)
  }
}

#' Claim Occurrence Times
#'
#' Returns the occurrence times of each of the claims occurring in each of
#' the periods, assuming the occurrence time of any claim in period \eqn{i}
#' is uniformly distributed between times \eqn{i - 1} and \eqn{i}.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @return A list of occurrence times such that the \eqn{i}th component of the
#' list gives the claim occurrence time for all claims that occurred in period
#' \eqn{i}.
#' @examples
#' n_vector <- c(90, 79, 102, 78, 86, 88, 116, 84, 93, 104)
#' # occurrence time for all claims originating from period 1
#' claim_occurrence(n_vector)[[1]]
#' @export
claim_occurrence <- function(frequency_vector) {
  I <- length(frequency_vector)
  occurrence_times <- vector("list", I)
  for (i in 1:I) {
    occurrence_times[[i]] <- stats::runif(frequency_vector[i], min = i - 1, max = i)
  }
  occurrence_times
}
