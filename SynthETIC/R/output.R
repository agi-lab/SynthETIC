###############################################################################
##                                   Output                                  ##
###############################################################################

#' Loss Reserving Output
#'
#' Outputs the full (or past) square of claim payments by occurrence period and
#' development period. The upper left triangle represents the past, and the
#' lower right triangle the unseen future. \cr \cr
#' Users can modify the aggregate level by providing an `aggregate_level`
#' argument to the function. For example, setting `aggregate_level = 4` when
#' working with calendar *quarters* produces a payment square by occurrence and
#' development *year*. \cr \cr
#' Users will also have the option to decide whether to include the out-of-bound
#' transactions to the maximum DQ, or to leave them in a separate "tail" cell,
#' see Details.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param payment_time_list (compound) list of payment times (both the continous
#' time scale and the discrete period versions work).
#' @param payment_size_list (compound) list of payment size pattern (can be
#' either with or without inflation).
#' @param aggregate_level number of periods to be aggregated together; must be
#' a divisor of the total number of periods under consideration (default 1).
#' @param incremental logical; if true returns the incremental payment square,
#' else returns the cumulative payment square.
#' @param future logical; if true shows the full claim triangle (i.e. including
#' claim payments in future periods), else shows only the past triangle (default
#' TRUE).
#' @param adjust logical; if true accumulates all payments beyond the max
#' development period to the last development period, else shows a separate
#' "tail" column for all the out-of-bound transactions.
#' @return An array of claims payments.
#' @details
#' **Remark on out-of-bound payment times**: This function allows adjustment
#' for out-of-bound transaction dates, by forcing payments that were projected
#' to fall out of the maximum development period to be paid at the exact end of
#' the maximum development period allowed (when we set `adjust = TRUE`, which is
#' the default behaviour). For example, if we consider 40 periods of development
#' and a claim incurred in the interval (20, 21] was projected to have a payment
#' at time 62.498210, then for the purpose of tabulation, we can
#' * treat such a payment as if it occurred at time 60 (`adjust = TRUE`);
#' * leave the payment in the "tail" cell, so the user can see the proportion of
#' payments beyond the maximum development period (`adjust = FALSE`).
#' @examples
#' attach(test_claims_object)
#' # a square of cumulative claims payments by accident and development quarters
#' CL <- claim_output(frequency_vector, payment_time_list, payment_size_list,
#'                    aggregate_level = 1, incremental = FALSE)
#' detach(test_claims_object)
#' @export
claim_output <- function(
  frequency_vector,
  payment_time_list,
  payment_size_list,
  aggregate_level = 1,
  incremental = TRUE,
  future = TRUE,
  adjust = TRUE) {

  I <- length(frequency_vector)
  if (adjust == TRUE) {
    # we will accumulate all out-of-bound transactions to the end of the max DQ
    output_incremental <- array(0, c(I, I))
    colnames(output_incremental) <- paste0("DP", 1:I)
    rownames(output_incremental) <- paste0("AP", 1:I)
  } else {
    # need one more column to track all payments beyond the max DQ
    output_incremental <- array(0, c(I, I + 1))
    colnames(output_incremental) <- c(paste0("DP", 1:I), "tail")
    rownames(output_incremental) <- paste0("AP", 1:I)
  }
  adjustment <- 0 # track the number of corrections required for keeping all the
                  # payments within the bound

  # convert to discrete time scale
  payment_period_list <- lapply(payment_time_list, lapply, ceiling)
  for (i in 1:I) {
    for (j in 1:frequency_vector[i]) {
      pmt <- payment_size_list[[i]][[j]]
      t <- payment_period_list[[i]][[j]] # t is a vector
      no_payment <- length(pmt)
      for (k in 1:no_payment) {
        if (t[k] - i + 1 > I) {
          # out-of-bound transactions
          adjustment <- adjustment + 1
          if (adjust == TRUE) {
            # ADJUSTMENT FOR OUT-OF-BOUND TRANSACTION TIMES
            t[k] <- i + I - 1
            output_incremental[i, t[k] - i + 1] <- output_incremental[i, t[k] - i + 1] + pmt[k]
          } else {
            # accumulate into the tail cell
            output_incremental[i, "tail"] <- output_incremental[i, "tail"] + pmt[k]
          }
        } else {
          output_incremental[i, t[k] - i + 1] <- output_incremental[i, t[k] - i + 1] + pmt[k]
        }

      }
    }
  }

  total_pmt_count <- length(unlist(payment_size_list))
  if (adjustment / total_pmt_count > 0.03) {
    warning("More than 3% of the payments were outside the bound.
    Check your notification and/or settlement delay assumptions!")
  }

  if (aggregate_level != 1) {
    # if aggregate at a higher level, sum together the values in individual squares
    # imagine dividing up a 40 x 40 square into 100 squares of size 4 x 4 to sum
    # quarterly data at a yearly level (but in a zig-zag fashion)
    new_side_length <- I / aggregate_level
    output_incremental_orig <- output_incremental

    if (adjust == TRUE) {
      output_incremental <- array(0, c(new_side_length, new_side_length))
      colnames(output_incremental) <- paste0("DP", 1:new_side_length)
      rownames(output_incremental) <- paste0("AP", 1:new_side_length)
    } else {
      output_incremental <- array(0, c(new_side_length, new_side_length + 1))
      colnames(output_incremental) <- c(paste0("DP", 1:new_side_length), "tail")
      rownames(output_incremental) <- paste0("AP", 1:new_side_length)
    }

    for (i in 1:new_side_length) {
      side_occurrence <- (aggregate_level*(i-1) + 1): (aggregate_level*i)
      for (j in 1:new_side_length) {
        side_development <- (aggregate_level*(j-1) + 1) : (aggregate_level*j)
        square_curr <- output_incremental_orig[side_occurrence, side_development]
        select <- apply(upper.tri(square_curr, diag = TRUE), 1, rev)
        if (j == 1) {
          output_incremental[i, j] <- sum(square_curr[select])
        } else {
          square_prev <- output_incremental_orig[
            side_occurrence, side_development - aggregate_level]
          output_incremental[i, j] <- sum(square_curr[select]) +
            sum(square_prev[!select])
        }
      }

      if (adjust == TRUE) {
        # accumulate everything to the last development period
        output_incremental[i, j] <- output_incremental[i, j] + sum(square_curr[!select])
      } else {
        output_incremental[i, "tail"] <-
          sum(square_curr[!select]) + sum(output_incremental_orig[side_occurrence, "tail"])
      }
    }
  }

  if (incremental == TRUE) {
    if (future == TRUE) {
      output_incremental
    } else {
      # only to show the past triangle
      # note that we won't have yet observed anything beyond the max DQ, so
      # we simply take the subset output_incremental[, 1:side]
      side <- nrow(output_incremental)
      past_incremental <- array(NA, c(side, side))
      colnames(past_incremental) <- paste0("DP", 1:side)
      rownames(past_incremental) <- paste0("AP", 1:side)
      indicator <- apply(upper.tri(output_incremental[, 1:side], diag = TRUE), 1, rev)
      for (i in 1:side) {
        past_incremental[i, 1:sum(indicator[i, ])] <- output_incremental[, 1:side][i, indicator[i, ]]
      }
      past_incremental
    }
  } else {
    output_cumulative <- output_incremental
    for (i in 1:dim(output_incremental)[1]) {
      output_cumulative[i, ] <- cumsum(output_incremental[i, ])
    }
    if (future == TRUE) {
      output_cumulative
    } else {
      # only to show the past triangle
      side <- nrow(output_cumulative)
      past_cumulative <- array(NA, c(side, side))
      colnames(past_cumulative) <- paste0("DP", 1:side)
      rownames(past_cumulative) <- paste0("AP", 1:side)
      indicator <- apply(upper.tri(output_cumulative[, 1:side], diag = TRUE), 1, rev)
      for (i in 1:side) {
        past_cumulative[i, 1:sum(indicator[i, ])] <- output_cumulative[, 1:side][i, indicator[i, ]]
      }
      past_cumulative
    }
  }

}


#' Generate a Claims Dataset
#'
#' Generates a dataset of claims records that takes the same structure as
#' \code{test_claim_dataset} included in this package, with each row
#' representing a unique claim.
#'
#' @param frequency_vector a vector of claim frequencies for all the periods.
#' @param occurrence_list list of claim occurrence times.
#' @param claim_size_list list of claim sizes.
#' @param notification_list list of notification delays.
#' @param settlement_list list of settlement delays.
#' @param no_payments_list list of number of partial payments.
#'
#' @return A dataframe that takes the same structure as
#' \code{\link{test_claim_dataset}}.
#' @examples
#' # demo only, in practice might generate claim dataset before simulating
#' # the partial payments
#' # this code generates the built-in test_claim_dataset
#' attach(test_claims_object)
#' claim_dataset <- generate_claim_dataset(
#'   frequency_vector, occurrence_list, claim_size_list, notification_list,
#'   settlement_list, no_payments_list
#' )
#' detach(test_claims_object)
#' @seealso \code{\link{test_claim_dataset}}
#'
#' @export
generate_claim_dataset <- function(
  frequency_vector,
  occurrence_list,
  claim_size_list,
  notification_list,
  settlement_list,
  no_payments_list) {

  I <- length(frequency_vector)
  claim_dataset <- cbind(occurrence_time = unlist(occurrence_list),
                         claim_size = unlist(claim_size_list),
                         notidel = unlist(notification_list),
                         setldel = unlist(settlement_list),
                         no_payment = unlist(no_payments_list))
  claim_dataset <- as.data.frame(claim_dataset)
  # Add a claim number that uniquely characterises each claim
  claim_dataset$claim_no <- seq_along(rownames(claim_dataset))
  # Attach occurrence period
  claim_dataset$occurrence_period <- as.numeric(rep(1:I, times = frequency_vector))
  # Reoder columns
  col_order <- c(
    "claim_no", "occurrence_period", "occurrence_time", "claim_size", "notidel",
    "setldel", "no_payment")

  claim_dataset[, col_order]
}


#' Generate a Transactions Dataset
#'
#' Generates a dataset of partial payment records that takes the same structure
#' as \code{test_transaction_dataset} included in this package, with each row
#' representing a unique payment.
#'
#' @param claims an `claims` object containing all the simulated quantities,
#' see \code{\link{claims}}.
#' @param adjust if TRUE then the payment times will be forced to match with the
#' maximum development period under consideration; default FALSE (which will
#' produce out-of-bound payment times).
#'
#' @return A dataframe that takes the same structure as
#' \code{\link{test_transaction_dataset}}.
#' @examples
#' # this generates the built-in test_transaction_dataset
#' transact_data <- generate_transaction_dataset(test_claims_object)
#'
#' @seealso \code{\link{test_transaction_dataset}}
#'
#' @export
generate_transaction_dataset <- function(claims, adjust = FALSE) {

  I <- length(claims$frequency_vector)
  claim_dataset <- generate_claim_dataset(
    claims$frequency_vector, claims$occurrence_list, claims$claim_size_list,
    claims$notification_list, claims$settlement_list, claims$no_payments_list)

  n_times <- unlist(claims$no_payments_list)
  transaction_dataset <- claim_dataset[rep(seq_len(nrow(claim_dataset)), n_times), ]
  rownames(transaction_dataset) <- NULL
  # Add payment number, payment size, inter-partial delay, payment period, and payment with inflation
  transaction_dataset$pmt_no <- unlist(sapply(n_times, seq, from = 1))
  transaction_dataset$pmt_no <- as.numeric(transaction_dataset$pmt_no)
  transaction_dataset <- cbind(transaction_dataset,
                               payment_size = unlist(claims$payment_size_list),
                               payment_delay = unlist(claims$payment_delay_list),
                               payment_time = unlist(claims$payment_time_list),
                               payment_inflated = unlist(claims$payment_inflated_list))
  transaction_dataset <- as.data.frame(transaction_dataset)

  # ADJUSTMENT FOR PAYMENT TIMES
  if (adjust == TRUE) {
    for (i in 1:nrow(transaction_dataset)) {
      if (transaction_dataset[i,"payment_time"] > transaction_dataset[i,"occurrence_period"] + I - 1) {
        transaction_dataset[i,"payment_time"] <- transaction_dataset[i,"occurrence_period"] + I - 1
      }
    }
  }
  transaction_dataset$payment_period <- ceiling(transaction_dataset$payment_time)

  # Reoder columns
  col_order <- c("claim_no", "pmt_no", "occurrence_period", "occurrence_time", "claim_size", "notidel",
                 "setldel", "payment_time", "payment_period", "payment_size", "payment_inflated", "payment_delay")

  transaction_dataset[, col_order]
}


#' Plot of Cumulative Claims Payments (Incurred Pattern)
#'
#' Generates a plot of cumulative claims paid (as a percentage of total amount
#' incurred) as a function of development time for each occurrence period.
#'
#' @param x an object of class `claims` containing all the simulated quantities.
#' @param by_year if `TRUE` returns a plot by occurrence year; otherwise returns
#' a plot by occurrence period (default).
#' @param inflated if `TRUE` shows a plot of payment pattern after inflation;
#' otherwise shows a plot of discounted payment pattern.
#' @param adjust if `TRUE` then the payment times will be forced to match with
#' the maximum development period under consideration, otherwise the plot will
#' see claims beyond the maximum development period; default `TRUE`.
#' @param ... other graphical parameters.
#' @seealso \code{\link{claims}}
#' @examples
#' plot(test_claims_object)
#' plot(test_claims_object, adjust = FALSE)
#' @importFrom ggplot2 ggplot aes
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot.claims <- function(
  x, by_year = FALSE, inflated = TRUE, adjust = TRUE, ...) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  transaction_dataset <- generate_transaction_dataset(x, adjust = FALSE)
  I <- max(transaction_dataset$occurrence_period)
  # ADJUSTMENT FOR PAYMENT TIMES
  if (adjust == TRUE) {
    for (i in 1:nrow(transaction_dataset)) {
      if (transaction_dataset[i,"payment_time"] > transaction_dataset[i,"occurrence_period"] + I - 1) {
        transaction_dataset[i,"payment_time"] <- transaction_dataset[i,"occurrence_period"] + I - 1
      }
    }
  }
  if (by_year == FALSE) {
    # percent = % paid of total claims incurred in the same period
    if (inflated == TRUE) {
      plot_data <- transaction_dataset %>%
        dplyr::group_by(.data$occurrence_period) %>%
        dplyr::mutate(percent = .data$payment_inflated/sum(.data$payment_inflated))
    } else {
      plot_data <- transaction_dataset %>%
        dplyr::group_by(.data$occurrence_period) %>%
        dplyr::mutate(percent = .data$payment_size/sum(.data$payment_size))
    }

    plot_data <- plot_data %>%
      dplyr::mutate(development = .data$payment_time - .data$occurrence_time) %>%
      dplyr::arrange(.data$occurrence_period, .data$development) %>%
      dplyr::group_by(.data$occurrence_period) %>%
      dplyr::mutate(cum_percent = cumsum(.data$percent))

    colour_count <- length(unique(plot_data$occurrence_period)) # number of levels
    get_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))
    ggplot(plot_data,
           aes(x = .data$development, y = .data$cum_percent * 100,
               colour = as.factor(.data$occurrence_period))) +
      ggplot2::geom_line() +
      ggplot2::scale_colour_manual(values = get_palette(colour_count), name = "Occurrence period") +
      ggplot2::labs(title = "Claims development for each of the occurrence periods",
                    x = "Development time (continuous scale)",
                    y = "Cumulative claim paid (%)") +
      # ggplot2::xlim(0, I) +
      ggplot2::guides(col = ggplot2::guide_legend(nrow = 2, byrow = TRUE,
                                                  title.position = "top",
                                                  title.hjust = 0.5)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     legend.title = ggplot2::element_text(face = "bold"),
                     legend.position = "bottom")
  } else {
    # plot by occurrence year
    if (inflated == TRUE) {
      plot_data_yearly <- transaction_dataset %>%
        dplyr::mutate(occurrence_year = ceiling(.data$occurrence_period * .pkgenv$time_unit)) %>%
        dplyr::group_by(.data$occurrence_year) %>%
        dplyr::mutate(percent = .data$payment_inflated/sum(.data$payment_inflated))
    } else {
      plot_data_yearly <- transaction_dataset %>%
        dplyr::mutate(occurrence_year = ceiling(.data$occurrence_period * .pkgenv$time_unit)) %>%
        dplyr::group_by(.data$occurrence_year) %>%
        dplyr::mutate(percent = .data$payment_size/sum(.data$payment_size))
    }

    plot_data_yearly <- plot_data_yearly %>%
      dplyr::mutate(development = (.data$payment_time - .data$occurrence_time) * .pkgenv$time_unit) %>%
      dplyr::arrange(.data$occurrence_year, .data$development) %>%
      dplyr::group_by(.data$occurrence_year) %>%
      dplyr::mutate(cum_percent = cumsum(.data$percent))

    colour_count <- length(unique(plot_data_yearly$occurrence_year)) # number of levels
    get_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))
    ggplot(plot_data_yearly,
           aes(x = .data$development, y = .data$cum_percent * 100,
               colour = as.factor(.data$occurrence_year))) +
      ggplot2::geom_line() +
      ggplot2::scale_colour_manual(values = get_palette(colour_count), name = "Occurrence year") +
      ggplot2::labs(title = "Claims development for each of the occurrence years",
                    x = "Development time (in years)",
                    y = "Cumulative claim paid (%)") +
      # ggplot2::xlim(0, I * .pkgenv$time_unit) +
      ggplot2::guides(col = ggplot2::guide_legend(nrow = 2, byrow = TRUE,
                                                  title.position = "top", title.hjust = 0.5)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     legend.title = ggplot2::element_text(face = "bold"),
                     legend.position = "bottom")
  }
}


#' Plot of Cumulative Claims Payments (Incurred Pattern)
#'
#' Generates a plot of cumulative claims paid (as a percentage of total amount
#' incurred) as a function of development time for each occurrence period.
#'
#' @param transactions a dataset of partial payment records.
#' @param occurrence_time_col name of column that stores the time of occurrence
#' of the claims (on a **continuous** scale).
#' @param payment_time_col name of column that stores the time of partial
#' payments of the claims (on a **continuous** scale).
#' @param payment_size_col name of column that stores the size of partial
#' payments of the claims.
#' @param by_year if `TRUE` returns a plot by occurrence year; otherwise returns
#' a plot by occurrence period (default).
#' @param adjust if `TRUE` then the payment times will be forced to match with
#' the maximum development period under consideration, otherwise the plot will
#' see claims beyond the maximum development period; default `TRUE`.
#' @seealso \code{\link{generate_transaction_dataset}}
#' @examples
#' plot_transaction_dataset(test_transaction_dataset)
#'
#' # Plot claim development without end-of-development-period correction
#' plot_transaction_dataset(test_transaction_dataset, adjust = FALSE)
#'
#' # Plot claim development without inflation effects
#' plot_transaction_dataset(test_transaction_dataset, payment_size_col = "payment_size")
#' @importFrom ggplot2 ggplot aes
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
plot_transaction_dataset <- function(
    transactions,
    occurrence_time_col = "occurrence_time",
    payment_time_col = "payment_time",
    payment_size_col = "payment_inflated",
    by_year = FALSE, adjust = TRUE)
{

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!occurrence_time_col %in% colnames(transactions)) {
    stop(paste(occurrence_time_col, "not in the provided dataset. Check your input!"))
  } else if (!payment_time_col %in% colnames(transactions)) {
    stop(paste(payment_time_col, "not in the provided dataset. Check your input!"))
  } else if (!payment_size_col %in% colnames(transactions)) {
    stop(paste(payment_size_col, "not in the provided dataset. Check your input!"))
  }

  transaction_dataset <- transactions %>%
    dplyr::mutate(.occurrence_period = ceiling(.data[[occurrence_time_col]]))
  I <- max(transaction_dataset$.occurrence_period)
  # ADJUSTMENT FOR PAYMENT TIMES
  if (adjust == TRUE) {
    for (i in 1:nrow(transaction_dataset)) {
      if (transaction_dataset[i, payment_time_col] > transaction_dataset[i, ".occurrence_period"] + I - 1) {
        transaction_dataset[i, payment_time_col] <- transaction_dataset[i, ".occurrence_period"] + I - 1
      }
    }
  }
  if (by_year == FALSE) {
    # percent = % paid of total claims incurred in the same period
    plot_data <- transaction_dataset %>%
      dplyr::group_by(.data$.occurrence_period) %>%
      dplyr::mutate(percent = .data[[payment_size_col]]/sum(.data[[payment_size_col]]))

    plot_data <- plot_data %>%
      dplyr::mutate(development = .data[[payment_time_col]] - .data[[occurrence_time_col]]) %>%
      dplyr::arrange(.data$.occurrence_period, .data$development) %>%
      dplyr::group_by(.data$.occurrence_period) %>%
      dplyr::mutate(cum_percent = cumsum(.data$percent))

    colour_count <- length(unique(plot_data$.occurrence_period)) # number of levels
    get_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))
    ggplot(plot_data,
           aes(x = .data$development, y = .data$cum_percent * 100,
               colour = as.factor(.data$.occurrence_period))) +
      ggplot2::geom_line() +
      ggplot2::scale_colour_manual(values = get_palette(colour_count), name = "Occurrence period") +
      ggplot2::labs(title = "Claims development for each of the occurrence periods",
                    x = "Development time (continuous scale)",
                    y = "Cumulative claim paid (%)") +
      # ggplot2::xlim(0, I) +
      ggplot2::guides(col = ggplot2::guide_legend(nrow = 2, byrow = TRUE,
                                                  title.position = "top",
                                                  title.hjust = 0.5)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     legend.title = ggplot2::element_text(face = "bold"),
                     legend.position = "bottom")
  } else {
    # plot by occurrence year
    plot_data_yearly <- transaction_dataset %>%
      dplyr::mutate(occurrence_year = ceiling(.data$.occurrence_period * .pkgenv$time_unit)) %>%
      dplyr::group_by(.data$occurrence_year) %>%
      dplyr::mutate(percent = .data[[payment_size_col]]/sum(.data[[payment_size_col]]))

    plot_data_yearly <- plot_data_yearly %>%
      dplyr::mutate(development = (.data[[payment_time_col]] - .data[[occurrence_time_col]]) * .pkgenv$time_unit) %>%
      dplyr::arrange(.data$occurrence_year, .data$development) %>%
      dplyr::group_by(.data$occurrence_year) %>%
      dplyr::mutate(cum_percent = cumsum(.data$percent))

    colour_count <- length(unique(plot_data_yearly$occurrence_year)) # number of levels
    get_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdYlBu"))
    ggplot(plot_data_yearly,
           aes(x = .data$development, y = .data$cum_percent * 100,
               colour = as.factor(.data$occurrence_year))) +
      ggplot2::geom_line() +
      ggplot2::scale_colour_manual(values = get_palette(colour_count), name = "Occurrence year") +
      ggplot2::labs(title = "Claims development for each of the occurrence years",
                    x = "Development time (in years)",
                    y = "Cumulative claim paid (%)") +
      # ggplot2::xlim(0, I * .pkgenv$time_unit) +
      ggplot2::guides(col = ggplot2::guide_legend(nrow = 2, byrow = TRUE,
                                                  title.position = "top", title.hjust = 0.5)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     legend.title = ggplot2::element_text(face = "bold"),
                     legend.position = "bottom")
  }
}
