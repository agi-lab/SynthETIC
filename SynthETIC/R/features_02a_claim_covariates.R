###############################################################################
##                            2a. Claim Covariates                           ##
###############################################################################

###############################################################################
# Covariates Object
###############################################################################

#' Construction of a `covariates` Object
#'
#' Constructs a `covariates` object which stores all covariate inputs.
#' All covariates will be assumed discrete.
#' Continuous covariates will have been discretized.
#'
#' @param factors named list of vectors, containing the name of the covariates
#' and associated factors in vector form.
#'
#' @return Returns a `covariates` object.
#'
#' @details
#' Creating a `covariates` object will provide template relativities for the
#' frequency and severity relativities. It is encouraged to use the setter
#' functions \code{\link{set.covariates_relativity}} to set these values to ensure that
#' all necessary inputs are provided.
#'
#' @examples
#' factors <- list(
#' "Legal Representation" = c("Y", "N"),
#' "Injury Severity" = as.character(1:6),
#' "Age of Claimant" = c("0-15", "15-30", "30-50", "50-65", "over 65")
#' )
#'
#' covariate_obj <- covariates(factors)
#' @export
covariates <- function(factors) {

  covariates <- list(
      factors = factors,
      relativity_freq = relativity_template(factors),
      relativity_sev = relativity_template(factors)
  )

  attr(covariates, "class") <- "covariates"
  covariates
}

#' @noRd
check_covariates_class <- function(covariates) {
    if (!isa(covariates, "covariates")) {
        stop("Input `covariates` is not of type `covariates`, see ?covariates.")
    }
}

#' Sets the claims relativity for a `covariates` object.
#'
#' @param covariates an object of type `covariates`, see \code{\link{covariates}}
#' @param relativity see \code{\link{relativity_template}}
#' @param freq_sev one of `"freq"` or `"sev"` to adjust frequency or severity
#'  relativities respectively
#'
#' @return Returns a `covariates` object.
#' @seealso \code{\link{covariates}}, \code{\link{relativity_template}}
#' @export
set.covariates_relativity <- function(covariates, relativity, freq_sev = c("freq", "sev")) {

    if (missing(freq_sev)) {
        stop('argument "freq_sev" is missing, with no default')
    }

    # Covariates check
    relativity <- as.data.frame(relativity)
    check_covariates_class(covariates)
    check_relativity(covariates$factors, relativity)

    # Set Frequency or Severity relativity
    if (freq_sev == "freq") {
        covariates$relativity_freq <- relativity
    } else if (freq_sev == "sev") {
        covariates$relativity_sev <- relativity
    } else {
        stop("Only relativity of type `freq` and `sev` is allowed.")
    }

    return (covariates)
}

###############################################################################
# Relativities
###############################################################################

#' Template to input Covariate Relativities
#'
#' Constructs a template for the covariate relativities based on the
#' inputed covariates. Note that only non-zero relativities and one cross-factor
#' relativity is needed for each factor pair.
#'
#' @param factors named list of vectors, containing the name of the covariates
#' and associated factors in vector form.
#'
#' @return Returns a dataframe object, with five columns:
#' \tabular{ll}{
#' `factor_i` \tab Factor i. \cr
#' `factor_j` \tab Factor j. \cr
#' `level_ik` \tab Level within Factor i. \cr
#' `level_jl` \tab Level within Factor j. \cr
#' `relativity` \tab Relativity between `level_ik` and `level_jl`, defaults to `NA`.
#' }
#'
#' @details
#'
#' Suppose that there are \eqn{m} covariates labelled \eqn{c_i}, \eqn{i=1,...,m},
#' and that covariate \eqn{c_i} can assume one and only one of \eqn{n_i} values,
#' \eqn{x_{ik}}, \eqn{k=1,...,n_i}. The total number of available covariate
#' values is \eqn{n = \sum_{i=1}^m n_o}.
#'
#' Now set up an \eqn{n \times n} matrix \eqn{F}, consisting of sub-matrices
#' \eqn{F_{ij}}, \eqn{i,j = 1, ... ,m} of dimension \eqn{n_i \times n_j}. The
#' diagonal blocks \eqn{F_{ii}} will quantify first-order relativities on claims
#' attributes, and the off-diagonal blocks \eqn{F_{ij}}, \eqn{j \neq i} will quantify
#' second-order effects. Let \eqn{f_{ij,kl}} denote the \eqn{(k, l)} element of
#' \eqn{F_{ij}}. This element operates as a multiplier of the claim attribute
#' when covariates \eqn{c_i} and \eqn{c_j} take values \eqn{x_{ik}} and \eqn{x_jl}
#' respectively. Since \eqn{c_i} can assume only one of the values \eqn{x_ik},
#' \eqn{f_{ii, kl} = 0} for \eqn{k \neq l}, and so \eqn{F_{ii}} is diagonal for
#' all \eqn{i=1, ..., m}. Moreover, \eqn{f_{ij,kl} = f_{ji,lk}}, so that
#' \eqn{F} is symmetric and \eqn{f_{ij,kl} > 0}.
#'
#' @example data-raw/example_relativity_template.R
#'
#' @export
relativity_template <- function(factors) {

    # Template for column names
    df_cols <- c("factor_i", "factor_j", "level_ik", "level_jl", "relativity")
    df <- data.frame(matrix(nrow = 0, ncol = length(df_cols)))
    colnames(df) <- df_cols

    cov_names <- names(factors)
    for (i in 1:length(factors)) {
        for (j in i:length(factors)) {
            factor_i = cov_names[i]
            factor_j = cov_names[j]
            level_ik = factors[[i]]
            level_jl = factors[[j]]

            if (factor_i == factor_j) {
                # No need for cross-factor relativities on the same covariate
                df_rows <- data.frame(
                    factor_i = factor_i,
                    factor_j = factor_j,
                    level_ik = level_ik,
                    level_jl = level_jl,
                    relativity = NA
                )
            } else {
                # Compute cross-factor relativities on different covariates
                df_rows <- data.frame(
                    factor_i = rep(factor_i, length(level_ik)*length(level_jl)),
                    factor_j = rep(factor_j, length(level_ik)*length(level_jl)),
                    level_ik = rep(level_ik, each = length(level_jl)),
                    level_jl = rep(level_jl, times = length(level_ik)),
                    relativity = NA
                )
            }

            df <- rbind(df, df_rows)
        }
    }

    return (df)
}

#' Function to check the input Covariate Relativities
#'
#' Performs assertions on inputted relativities, will raise an error if any checks
#' fail. Currently checks on the: necessary column names used in dataframe,
#' required factors and levels based on inputted `factors`, inputted relativities
#' being non-negative numbers.
#'
#' @param factors named list of vectors, containing the name of the covariates
#' and associated factors in vector form.
#' @param relativity relativity dataframe as defined in \code{\link{relativity_template}}.
#'
#' @export
check_relativity <- function(factors, relativity) {

    template <- relativity_template(factors)

    # Check that necessary columns are in
    if (!all(names(template) %in% names(relativity))) {
        stop("Ensure all required columns are in relativity, see `?relativity_template`.")
    }

    # Vectors of required factors
    factor_levels_req_1 <- base::apply(
        X = template,
        MARGIN = 1,
        FUN = function(x) paste(
            x[["factor_i"]],
            x[["factor_j"]],
            x[["level_ik"]],
            x[["level_jl"]],
            sep = " - "
        )
    )
    factor_levels_req_2 <- base::apply(
        X = template,
        MARGIN = 1,
        FUN = function(x) paste(
            x[["factor_j"]],
            x[["factor_i"]],
            x[["level_jl"]],
            x[["level_ik"]],
            sep = " - "
        )
    )

    # Provided Factor levels
    factor_levels_prov <- base::apply(
        X = relativity,
        MARGIN = 1,
        FUN = function(x) paste(
            x[["factor_i"]],
            x[["factor_j"]],
            x[["level_ik"]],
            x[["level_jl"]],
            sep = " - "
        )
    )

    # Check that all necessary combinations of factors-levels are included
    condition_1 <- factor_levels_req_1 %in% factor_levels_prov
    condition_2 <- factor_levels_req_2 %in% factor_levels_prov
    missing_factors <- !(condition_1 | condition_2)
    if (any(missing_factors)) {
        missing_levels <- factor_levels_req_1[missing_factors]
        missing_levels <- paste0(missing_levels, collapse = "\n ")

        stop(paste("Relativity table is not complete, see `?relativity_template`.
             \nMissing levels include:\n", missing_levels))
    }

    if (!all(is.numeric(relativity[["relativity"]])) |
        any(relativity[["relativity"]] < 0)) {
        stop("Ensure all relativities are inputted correctly, see `?relativity_template`.")
    }

}

###############################################################################
# Covariates Dataset
###############################################################################

#' Construction of a `covariates_data` Object
#'
#' Constructs a `covariates_data` object which stores the dataset of known covariate levels of each factor.
#'
#' @param covariates a \code{\link{covariates}} object
#' @param data a dataset of covariate values, with columns equal to each of the covariate factors and rows related to individual claim observations.
#' @param covariates_id optional list of list of ids, in the same format as a \code{\link{claim_size}} output. Also see \code{\link{to_SynthETIC}}. Defaults to \code{NULL}.
#'
#' @return Returns a `covariates_data` object.
#'
#' @export
covariates_data <- function(covariates, data, covariates_id = NULL) {

    data <- as.data.frame(data)
    check_covariates_data(covariates, data, covariates_id)

    covariates_data <- list(
        covariates = covariates,
        data = data,
        ids =  covariates_id
    )

    attr(covariates_data, "class") <- "covariates_data"
    covariates_data
}

#' @noRd
check_covariates_data <- function(covariates, data, covariates_id) {

    check_covariates_class(covariates)

    # Check all factors in covariates appear in data
    if (!all(names(data) %in% names(covariates$factors))) {
        stop("Covariates data is missing covariate factors.")
    }

    # Check all levels for each factor
    check_levels <- function(i) {
        if(!all(data[, i] %in% covariates$factors[[names(data)[i]]])) {
            stop("Not all levels in data present in covariate factor.")
        }
    }

    temp_ <- sapply(
        X = 1:length(data),
        FUN = check_levels
    )

    # Check that all ids are present in data
    if (!all(unlist(covariates_id) %in% rownames(data))) {
        stop("Not all covariate ids are present in data.")
    }
}

###############################################################################
# Calculating observation level relativities
###############################################################################

#' Calculates Relativities
#'
#' Calculates the relativities (`freq` or `sev`) of a set of covariate values.
#'
#' @param covariates_data a \code{\link{covariates_data}} object
#' @param freq_sev one of `freq` or `sev` to calculate the frequency or severity relativity respectively.
#' @param by_ids optional boolean to calculate reorder the result based off claim observations instead of observations in the covariates dataset. Defaults to FALSE.
#'
#' @export
covariates_relativity <- function(covariates_data, freq_sev = c("freq", "sev"),
                                  by_ids = FALSE) {

    if (!isa(covariates_data, "covariates_data")) {
        stop("Input `covariates_data` is not of type `covariates_data`, see ?covariates_data")
    }
    if (missing(freq_sev)) {
        stop('argument "relativity" is missing, with no default')
    }
    if (freq_sev == "freq") {
        relativity <- covariates_data$covariates$relativity_freq
    } else if (freq_sev == "sev") {
        relativity <- covariates_data$covariates$relativity_sev
    } else {
        stop("Only relativity of type `freq` and `sev` is allowed.")
    }
    if (by_ids & is.null(covariates_data$ids)) {
        warning("No ids found in `covariates_data` when `by_ids` is TRUE.")
        by_ids <- FALSE
    }

    cov_names <- names(covariates_data$covariates$factors)
    # Calculates the relativity for one observation
    calc_relativity <- function(covariates_x_1d) {
        rel <- 1
        for (i in 1:length(cov_names)) {
            for (j in i:length(cov_names)) {
                fij_kikj <- relativity[
                    relativity$factor_i == cov_names[i] &
                        relativity$factor_j == cov_names[j] &
                        relativity$level_ik == covariates_x_1d[[cov_names[i]]] &
                        relativity$level_jl == covariates_x_1d[[cov_names[j]]],
                    "relativity"
                ]

                rel <- rel * fij_kikj
            }
        }
        return (rel)
    }

    relativities <- apply(
        X = covariates_data$data,
        MARGIN = 1,
        FUN = calc_relativity
    )

    # Reorder output in the same list as ids.
    if (by_ids) {
        ids_index <- match(
            unlist(covariates_data$ids),
            rownames(covariates_data$data)
        )
        relativities <- relativities[ids_index]
    }

    return (relativities)
}

###############################################################################
# Covariates Simulation
###############################################################################

#' Covariates Simulation
#'
#' Simulates covariates for each claim. The relative occurrence of each combination of covariates is given the frequency relativities of the covariates.
#'
#' @param covariates a \code{\link{covariates}} object, which stores the frequency and severity relativities for given covariate levels.
#' @param frequency_vector a vector in the same output as \code{\link{claim_frequency}}.
#' @param claim_size_list optional if `frequency_vector` is not inputted. A list in the same output as \code{\link{claim_size}}.
#' @param random_seed optional seed for random number generation for
#' reproducibility.
#'
#' @return Returns a \code{\link{covariates_data}} object.
#' @export
simulate_covariates <- function(
        covariates,
        frequency_vector = 1,
        claim_size_list = list(1),
        random_seed = NULL
) {
    set.seed(random_seed)

    if (!missing(frequency_vector) & !missing(claim_size_list)) {
        stop("specify 'frequency_vector' or 'claim_size_list' but not both")
    } else {
        if (!missing(claim_size_list)) {
            frequency_vector <- unlist(lapply(claim_size_list, length))
        }

        n <- sum(frequency_vector)
    }

    check_covariates_class(covariates)
    all_combinations <- base::expand.grid(covariates$factors)
    temp_covariates_data <- covariates_data(
        covariates = covariates,
        data = all_combinations,
        covariates_id = NULL
    )
    all_relativities <- covariates_relativity(temp_covariates_data, freq_sev = "freq")

    covariates_sim <- stats::rmultinom(n, 1, all_relativities)
    covariates_id <- base::apply(
        covariates_sim,
        MARGIN = 2,
        FUN = function(x) match(1, x)
    )

    covariates_x <- all_combinations[covariates_id, ]
    rownames(covariates_x) <- NULL

    covariates_data <- covariates_data(
        covariates = covariates,
        data = covariates_x,
        covariates_id = to_SynthETIC(1:n, frequency_vector)
    )

    return (covariates_data)
}

###############################################################################
# Covariates Claim Size Adjustment
###############################################################################

#' Covariates Claim Size Adjustment
#'
#' Adjusts claim sizes given a set of covariates. Note that this function firstly simulates covariate levels for each claim, see \code{\link{simulate_covariates}}.
#'
#' @param covariate_obj a \code{\link{covariates}} object
#' @param claim_size a list in the same output as \code{\link{claim_size}}
#' @param random_seed optional seed for random number generation for
#' reproducibility.
#'
#' @return Returns a nested named list:
#' - `covariates_data` which is a named list of covariate relativities (\code{\link{covariates}}), the simulated covariate levels (`data`) and the claim IDs.
#' - `claim_size_adj` which is a list of adjusted claim sizes such that the \eqn{i}th component of the list gives the sizes for all claims that occurred in period \eqn{i}.
#'
#' @export
claim_size_adj <- function(covariate_obj, claim_size, random_seed = NULL) {

    covariates_data <- simulate_covariates(
        covariate_obj,
        claim_size_list = claim_size,
        random_seed = random_seed
    )

    claim_size_adj <- claim_size_adj.fit(covariates_data, claim_size)

    z <- list(
        covariates_data = covariates_data,
        claim_size_adj = claim_size_adj
    )

    z
}

#' Covariates Claim Size Adjustment
#'
#' Adjusts claim sizes given the covariates related to each claim. The relative adjustment of each claim size is given by the severity relativities of the covariates.
#'
#' @param covariates_data a `covariates_data` object
#' @param claim_size a list in the same output as \code{\link{claim_size}}
#'
#' @export
claim_size_adj.fit <- function(covariates_data, claim_size) {
    # Check covariates_data of correct object
    if (!isa(covariates_data, "covariates_data")) {
        stop("Input `covariates_data` is not of type `covariates_data`, see ?covariates_data")
    }

    # Check that claim_ids list can be mapped to claim_size list
    if (!all(sapply(covariates_data$ids, length) == sapply(claim_size, length))){
        stop("`covariates_data` ids list cannot be mapped to claim_size list.")
    }

    id_vector <- unlist(covariates_data$ids)
    claim_size_vector <- unlist(claim_size)
    claim_relativities <- covariates_relativity(
        covariates_data,
        freq_sev = "sev",
        by_ids = TRUE
    )

    claim_sizes_scaled <- claim_size_vector * claim_relativities
    claim_size_adj <- claim_sizes_scaled * sum(claim_size_vector) / sum(claim_sizes_scaled)
    claim_size_adj <- to_SynthETIC(
        claim_size_adj,
        sapply(covariates_data$ids, length)
    )
    return (claim_size_adj)
}
