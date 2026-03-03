#' Plot an epidemic curve
#'
#' Creates a bar chart of incidence over time.
#'
#' @param data A data frame with date and incidence columns.
#' @param date_col Name of date column.
#' @param count_col Name of incidence column.
#' @param fill Fill color for bars.
#'
#' @return A `ggplot2` object.
#' @export
plot_epicurve <- function(data, date_col = "date", count_col = "incidence", fill = "#2C7FB8") {
  incid_tbl <- prep_incidence_data(
    data = data,
    date_col = date_col,
    count_col = count_col,
    complete_dates = TRUE
  )

  ggplot2::ggplot(incid_tbl, ggplot2::aes(x = .data$date, y = .data$incidence)) +
    ggplot2::geom_col(fill = fill, alpha = 0.9) +
    ggplot2::labs(
      x = "Date",
      y = "Incidence",
      title = "Epidemic Curve"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
