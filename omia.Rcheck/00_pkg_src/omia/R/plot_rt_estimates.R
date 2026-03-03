#' Plot time-varying reproduction number estimates
#'
#' Plots mean `R_t` with 95% uncertainty band.
#'
#' @param rt_data A tibble, typically from [estimate_rt_epiestim()].
#' @param date_col Name of date column.
#' @param mean_col Name of mean Rt column.
#' @param lower_col Name of lower interval column.
#' @param upper_col Name of upper interval column.
#'
#' @return A `ggplot2` object.
#' @export
plot_rt_estimates <- function(
    rt_data,
    date_col = "date_end",
    mean_col = "mean_r",
    lower_col = "lower_95",
    upper_col = "upper_95"
) {
  validate_required_cols(rt_data, c(date_col, mean_col, lower_col, upper_col), arg = "rt_data")

  date_sym <- rlang::sym(date_col)
  mean_sym <- rlang::sym(mean_col)
  lower_sym <- rlang::sym(lower_col)
  upper_sym <- rlang::sym(upper_col)

  rt_tbl <- rt_data |>
    dplyr::transmute(
      date = as_date_safe(!!date_sym, date_col),
      mean_r = as.numeric(!!mean_sym),
      lower_95 = as.numeric(!!lower_sym),
      upper_95 = as.numeric(!!upper_sym)
    )

  ggplot2::ggplot(rt_tbl, ggplot2::aes(x = .data$date, y = .data$mean_r)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower_95, ymax = .data$upper_95),
      fill = "#9ECAE1",
      alpha = 0.5
    ) +
    ggplot2::geom_line(color = "#08519C", linewidth = 0.8) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#B30000") +
    ggplot2::labs(
      x = "Date",
      y = expression(R[t]),
      title = "Time-varying Reproduction Number"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
