#' Fit a log-linear Poisson growth model
#'
#' Fits a Poisson regression of incidence on time and reports growth rate,
#' confidence interval, and doubling time.
#'
#' @param data A data frame with date and incidence columns.
#' @param date_col Name of date column.
#' @param count_col Name of incidence/count column.
#'
#' @return A list with model object, coefficient summary tibble, and fitted
#'   values tibble.
#' @export
fit_poisson_growth <- function(data, date_col = "date", count_col = "incidence") {
  incid_tbl <- prep_incidence_data(
    data = data,
    date_col = date_col,
    count_col = count_col,
    complete_dates = TRUE
  )

  model_df <- incid_tbl |>
    dplyr::mutate(day = as.numeric(.data$date - min(.data$date)) + 1)

  fit <- stats::glm(
    incidence ~ day,
    family = stats::poisson(link = "log"),
    data = model_df
  )

  est <- stats::coef(fit)[["day"]]
  se <- sqrt(diag(stats::vcov(fit)))[["day"]]
  z <- stats::qnorm(0.975)
  lower <- est - z * se
  upper <- est + z * se

  doubling_time <- if (isTRUE(all.equal(est, 0, tolerance = 1e-12))) {
    Inf
  } else {
    log(2) / est
  }

  estimates <- tibble::tibble(
    term = "day",
    growth_rate = est,
    std_error = se,
    conf_low = lower,
    conf_high = upper,
    doubling_time = doubling_time
  )

  fitted_values <- model_df |>
    dplyr::mutate(
      fitted = stats::fitted(fit),
      residual = stats::residuals(fit, type = "pearson")
    )

  list(
    model = fit,
    estimates = estimates,
    fitted = fitted_values
  )
}
