#' Forecast incidence from a Poisson growth model with uncertainty
#'
#' Fits a Poisson log-linear growth model and propagates coefficient
#' uncertainty into short-term forecasts, with optional Poisson observation
#' noise.
#'
#' @param data A data frame with date and incidence columns.
#' @param date_col Name of date column.
#' @param count_col Name of incidence/count column.
#' @param horizon Forecast horizon in days.
#' @param level Central prediction interval level (e.g. `0.9`).
#' @param n_draws Number of forecast draws.
#' @param include_observation_uncertainty If `TRUE`, sample Poisson outcomes
#'   around expected incidence. If `FALSE`, return expected-value draws only.
#' @param mean_si Mean serial interval passed to [fit_poisson_growth()] for
#'   implied `R_t` reporting.
#' @param seed Optional random seed.
#'
#' @return A list with components:
#'   * `forecast`: day-level summary (`date`, `day_ahead`, `mean_expected`,
#'     `median_forecast`, `lower`, `upper`, `level`),
#'   * `draws`: matrix of predictive draws (`n_draws` x `horizon`),
#'   * `expected_draws`: matrix of expected-incidence draws,
#'   * `fit`: output from [fit_poisson_growth()].
#' @export
forecast_poisson_growth <- function(
    data,
    date_col = "date",
    count_col = "incidence",
    horizon = 7L,
    level = 0.9,
    n_draws = 2000L,
    include_observation_uncertainty = TRUE,
    mean_si = 4.7,
    seed = NULL
) {
  if (!is.numeric(horizon) || length(horizon) != 1L ||
      horizon < 1 || horizon != as.integer(horizon)) {
    rlang::abort("`horizon` must be a positive integer.")
  }

  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
      level <= 0 || level >= 1) {
    rlang::abort("`level` must be a single number in (0, 1).")
  }

  if (!is.numeric(n_draws) || length(n_draws) != 1L ||
      n_draws < 10 || n_draws != as.integer(n_draws)) {
    rlang::abort("`n_draws` must be an integer >= 10.")
  }

  incid_tbl <- prep_incidence_data(
    data = data,
    date_col = date_col,
    count_col = count_col,
    complete_dates = TRUE
  )

  fit <- fit_poisson_growth(
    data = incid_tbl,
    date_col = "date",
    count_col = "incidence",
    n_draws = 0L,
    mean_si = mean_si
  )

  beta <- stats::coef(fit$model)
  vc <- stats::vcov(fit$model)

  if (!all(c("(Intercept)", "day") %in% names(beta))) {
    rlang::abort("Poisson growth model coefficients not found.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  z <- matrix(stats::rnorm(n_draws * 2L), ncol = 2L)
  chol_vc <- tryCatch(chol(vc[c("(Intercept)", "day"), c("(Intercept)", "day")]),
    error = function(e) NULL
  )

  if (is.null(chol_vc)) {
    # Fallback to independent Normal approximation when covariance is singular
    beta_draws <- cbind(
      "(Intercept)" = stats::rnorm(
        n_draws,
        mean = beta[["(Intercept)"]],
        sd = sqrt(vc["(Intercept)", "(Intercept)"])
      ),
      "day" = stats::rnorm(
        n_draws,
        mean = beta[["day"]],
        sd = sqrt(vc["day", "day"])
      )
    )
  } else {
    beta_draws <- z %*% chol_vc
    beta_draws <- sweep(beta_draws, 2, beta[c("(Intercept)", "day")], "+")
  }

  day_hist <- as.numeric(max(incid_tbl$date) - min(incid_tbl$date)) + 1
  day_future <- day_hist + seq_len(horizon)

  eta <- outer(beta_draws[, 1], rep(1, horizon)) +
    outer(beta_draws[, 2], day_future)
  mu_draws <- exp(eta)

  pred_draws <- if (isTRUE(include_observation_uncertainty)) {
    matrix(stats::rpois(length(mu_draws), lambda = as.numeric(mu_draws)),
      nrow = n_draws,
      ncol = horizon
    )
  } else {
    mu_draws
  }

  alpha <- 1 - level
  lower_p <- alpha / 2
  upper_p <- 1 - alpha / 2

  forecast_tbl <- tibble::tibble(
    day_ahead = seq_len(horizon),
    date = max(incid_tbl$date) + seq_len(horizon),
    mean_expected = colMeans(mu_draws),
    median_forecast = apply(pred_draws, 2, stats::median),
    lower = apply(pred_draws, 2, stats::quantile, probs = lower_p, names = FALSE, type = 8),
    upper = apply(pred_draws, 2, stats::quantile, probs = upper_p, names = FALSE, type = 8),
    level = level
  )

  list(
    forecast = forecast_tbl,
    draws = pred_draws,
    expected_draws = mu_draws,
    fit = fit
  )
}
