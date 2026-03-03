#' Fit a log-linear Poisson growth model
#'
#' Fits a Poisson regression of incidence on time and reports growth rate,
#' confidence interval, and doubling time.
#'
#' @param data A data frame with date and incidence columns.
#' @param date_col Name of date column.
#' @param count_col Name of incidence/count column.
#' @param n_draws Number of uncertainty draws from the asymptotic Normal
#'   distribution of the growth coefficient. Set to `0` to skip.
#' @param mean_si Mean serial interval used to map growth-rate draws to
#'   implied `R_t` draws via `exp(growth_rate * mean_si)`.
#' @param seed Optional random seed used when `n_draws > 0`.
#'
#' @return A list with model object, coefficient summary tibble, fitted values,
#'   and (optionally) growth-rate uncertainty draws.
#' @export
fit_poisson_growth <- function(
    data,
    date_col = "date",
    count_col = "incidence",
    n_draws = 0L,
    mean_si = 4.7,
    seed = NULL
) {
  if (!is.numeric(n_draws) || length(n_draws) != 1L ||
      n_draws < 0 || n_draws != as.integer(n_draws)) {
    rlang::abort("`n_draws` must be a non-negative integer.")
  }

  if (!is.numeric(mean_si) || length(mean_si) != 1L || !is.finite(mean_si) || mean_si <= 0) {
    rlang::abort("`mean_si` must be a single positive finite number.")
  }

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
    doubling_time = doubling_time,
    implied_rt = exp(est * mean_si)
  )

  fitted_values <- model_df |>
    dplyr::mutate(
      fitted = stats::fitted(fit),
      residual = stats::residuals(fit, type = "pearson")
    )

  uncertainty_draws <- NULL
  if (n_draws > 0) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    growth_draw <- stats::rnorm(n_draws, mean = est, sd = se)
    doubling_draw <- dplyr::if_else(
      abs(growth_draw) < 1e-12,
      Inf,
      log(2) / growth_draw
    )

    uncertainty_draws <- tibble::tibble(
      draw = seq_len(n_draws),
      growth_rate = growth_draw,
      doubling_time = doubling_draw,
      implied_rt = exp(growth_draw * mean_si)
    )
  }

  list(
    model = fit,
    estimates = estimates,
    fitted = fitted_values,
    uncertainty_draws = uncertainty_draws
  )
}
