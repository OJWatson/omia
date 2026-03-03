#' Evaluate forecasts with rolling-origin backtesting
#'
#' Repeatedly refits [forecast_poisson_growth()] on expanding windows and
#' evaluates short-term out-of-sample performance.
#'
#' @param data A data frame with date and incidence columns.
#' @param date_col Name of date column.
#' @param count_col Name of incidence/count column.
#' @param initial_window Initial training-window length.
#' @param horizon Forecast horizon in days.
#' @param step Step size between forecast origins.
#' @param level Central prediction interval level for coverage.
#' @param n_draws Number of forecast draws per origin.
#' @param include_observation_uncertainty Whether to include Poisson
#'   observation uncertainty in forecast draws.
#' @param mean_si Mean serial interval passed to [forecast_poisson_growth()].
#' @param wis_levels Central interval levels used in WIS.
#' @param seed Optional random seed.
#'
#' @return A list with:
#'   * `scores`: day-level scores by origin/day,
#'   * `metrics_overall`: aggregated overall metrics,
#'   * `metrics_by_horizon`: metrics aggregated by forecast horizon day,
#'   * `forecast_summaries`: concatenated forecast summaries.
#' @export
evaluate_rolling_origin_forecast <- function(
    data,
    date_col = "date",
    count_col = "incidence",
    initial_window,
    horizon = 7L,
    step = 1L,
    level = 0.9,
    n_draws = 1000L,
    include_observation_uncertainty = TRUE,
    mean_si = 4.7,
    wis_levels = c(0.5, 0.8, 0.95),
    seed = NULL
) {
  if (!is.numeric(initial_window) || length(initial_window) != 1L ||
      initial_window < 2 || initial_window != as.integer(initial_window)) {
    rlang::abort("`initial_window` must be an integer >= 2.")
  }

  if (!is.numeric(horizon) || length(horizon) != 1L ||
      horizon < 1 || horizon != as.integer(horizon)) {
    rlang::abort("`horizon` must be a positive integer.")
  }

  if (!is.numeric(step) || length(step) != 1L ||
      step < 1 || step != as.integer(step)) {
    rlang::abort("`step` must be a positive integer.")
  }

  incid_tbl <- prep_incidence_data(
    data = data,
    date_col = date_col,
    count_col = count_col,
    complete_dates = TRUE
  )

  y <- incid_tbl$incidence
  dates <- incid_tbl$date
  n <- length(y)

  if (initial_window + horizon > n) {
    rlang::abort("Not enough data for requested `initial_window` + `horizon`.")
  }

  origins <- seq(from = initial_window, to = n - horizon, by = step)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  score_list <- vector("list", length(origins))
  fc_list <- vector("list", length(origins))

  for (i in seq_along(origins)) {
    origin_idx <- origins[[i]]
    train_df <- incid_tbl[seq_len(origin_idx), , drop = FALSE]
    obs <- y[seq.int(origin_idx + 1L, origin_idx + horizon)]

    fc <- forecast_poisson_growth(
      data = train_df,
      date_col = "date",
      count_col = "incidence",
      horizon = horizon,
      level = level,
      n_draws = n_draws,
      include_observation_uncertainty = include_observation_uncertainty,
      mean_si = mean_si
    )

    draws_tbl <- tibble::tibble(
      sim = rep(seq_len(nrow(fc$draws)), times = ncol(fc$draws)),
      day = rep(seq_len(ncol(fc$draws)), each = nrow(fc$draws)),
      incidence = as.numeric(fc$draws)
    )

    sc <- score_forecast_draws(
      observed = obs,
      forecast_draws = draws_tbl,
      point = "median",
      interval_level = level,
      wis_levels = wis_levels
    ) |>
      dplyr::mutate(
        origin_index = origin_idx,
        origin_date = dates[[origin_idx]],
        day_ahead = .data$day,
        target_date = dates[origin_idx + .data$day]
      )

    fc_sum <- fc$forecast |>
      dplyr::mutate(
        origin_index = origin_idx,
        origin_date = dates[[origin_idx]]
      )

    score_list[[i]] <- sc
    fc_list[[i]] <- fc_sum
  }

  scores <- dplyr::bind_rows(score_list)
  forecast_summaries <- dplyr::bind_rows(fc_list)

  metrics_overall <- scores |>
    dplyr::summarise(
      mae = mean(.data$abs_error),
      rmse = sqrt(mean(.data$sq_error)),
      interval_coverage = mean(.data$covered),
      wis = mean(.data$wis),
      crps = mean(.data$crps),
      n_origins = dplyr::n_distinct(.data$origin_index),
      n_forecasts = dplyr::n()
    )

  metrics_by_horizon <- scores |>
    dplyr::group_by(.data$day_ahead) |>
    dplyr::summarise(
      mae = mean(.data$abs_error),
      rmse = sqrt(mean(.data$sq_error)),
      interval_coverage = mean(.data$covered),
      wis = mean(.data$wis),
      crps = mean(.data$crps),
      .groups = "drop"
    )

  list(
    scores = scores,
    metrics_overall = metrics_overall,
    metrics_by_horizon = metrics_by_horizon,
    forecast_summaries = forecast_summaries
  )
}
