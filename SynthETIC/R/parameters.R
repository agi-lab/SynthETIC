###############################################################################
##                       Packagewise Global Paramaters                       ##
###############################################################################

.pkgenv <- new.env(parent = emptyenv())
# default values
.pkgenv$ref_claim <- 200000
.pkgenv$time_unit <- 1/4 # quarters

#' Set Packagewise Global Parameters for the Claims Simulator
#'
#' Sets `ref_claim` and `time_unit` parameters for all the simulation functions
#' within the `SynthETIC` package.
#'
#' @details Those variables will be available to multiple functions in this
#' package, but are kept local to the package environment (i.e. not accessible
#' from the global environment). To extract the current values of the variables,
#' use \code{\link{return_parameters}}. \cr \cr
#' We introduce the reference value `ref_claim` partly as a measure of the
#' monetary unit and/or overall claims experience. The default distributional
#' assumptions were set up with an Australian Auto Liability portfolio in mind.
#' `ref_claim` then allows users to easily simulate a synthetic portfolio with
#' similar claim pattern but in a different currency, for example. We also
#' remark that users can alternatively choose to interpret `ref_claim` as a
#' monetary unit. For example, one can set `ref_claim <- 1000` and think of all
#' amounts in terms of $1,000. However, in this case the default simulation
#' functions will not work and users will need to supply their own set of
#' functions and set the values as multiples of `ref_claim` rather than
#' fractions as in the default setting. \cr \cr
#' We also require the user to input a `time_unit` (which should be given as a
#' fraction of year), so that the default input parameters apply to contexts
#' where the time units are no longer in quarters. In the default setting we
#' have a `time_unit` of 1/4 i.e. we work with calendar quarters. \cr \cr
#' The default input parameters will update automatically with the choice of
#' the two variables `ref_claim` and `time_unit`, which ensures that the
#' simulator produce sensible results in contexts other than the default
#' setting. We remark that both `ref_claim` and `time_unit` only affect the
#' default simulation functions, and users can also choose to set up their own
#' modelling assumptions for any of the modules to match their experiences even
#' better. In the latter case, it is the responsibility of the user to ensure
#' that their input parameters are compatible with their time units and claims
#' experience. For example, if the time units are quarters, then claim
#' occurrence rates must be quarterly.
#' @seealso See the vignette for this package for a full list of functions
#' impacted by those two variables.
#' @param ref_claim a reference value for the claim sizes (default 200000).
#' @param time_unit time unit to work with, given as a fraction of a year;
#' default calendar quarters (1/4).
#' @examples
#' set_parameters(ref_claim = 200000, time_unit = 1/12) # monthly reserving
#' @export
set_parameters <- function(ref_claim = 200000, time_unit = 1/4) {
  assign('ref_claim', ref_claim, envir = .pkgenv)
  assign('time_unit', time_unit, envir = .pkgenv)
}

#' Get Current Parameters
#'
#' Returns the current values of `ref_claim` and `time_unit` parameters, two
#' packagewise-global variables used by all simulation functions within this
#' package.
#'
#' @details Returns and (optionally) prints the current values of `ref_claim`
#' and `time_unit` parameters.
#' @param print logical; if TRUE prints a message.
#' @seealso \code{\link{set_parameters}}
#' @examples
#' cur <- return_parameters()
#' cur
#' set_parameters(ref_claim = 200000, time_unit = 1/12) # monthly reserving
#' return_parameters(print = FALSE)
#' @export
return_parameters <- function(print = FALSE) {
  if (print == TRUE) {
    print(paste('The current parameters are: ref_claim is set at', .pkgenv$ref_claim,
                'and time_unit is set at', .pkgenv$time_unit))
  }

  c(.pkgenv$ref_claim, .pkgenv$time_unit)
}
