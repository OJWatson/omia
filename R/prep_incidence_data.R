#' Prepare incidence data
#'
#' Standardizes and validates daily incidence data for downstream analysis.
#'
#' @param data A data frame containing at least a date and incidence column.
#' @param date_col Name of the date column.
#' @param count_col Name of the incidence/count column.
#' @param complete_dates If `TRUE`, fill missing dates with `fill_value`.
#' @param fill_value Value used for missing dates when `complete_dates = TRUE`.
#'
#' @return A tibble with columns `date` and `incidence`.
#' @export
prep_incidence_data <- function(
    data,
    date_col = "date",
    count_col = "incidence",
    complete_dates = TRUE,
    fill_value = 0
) {
  validate_required_cols(data, c(date_col, count_col), arg = "data")

  date_sym <- rlang::sym(date_col)
  count_sym <- rlang::sym(count_col)

  out <- dplyr::transmute(
    data,
    date = as_date_safe(!!date_sym, date_col),
    incidence = as.numeric(!!count_sym)
  )

  validate_nonnegative_numeric(out$incidence, "incidence")

  out <- out |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(incidence = sum(.data$incidence, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$date)

  if (isTRUE(complete_dates)) {
    if (!is.numeric(fill_value) || length(fill_value) != 1L || !is.finite(fill_value) || fill_value < 0) {
      rlang::abort("`fill_value` must be a single non-negative finite numeric value.")
    }

    out <- out |>
      tidyr::complete(
        date = seq(min(.data$date), max(.data$date), by = "day"),
        fill = list(incidence = fill_value)
      ) |>
      dplyr::arrange(.data$date)
  }

  tibble::as_tibble(out)
}
