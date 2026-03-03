#' Simulate renewal-based count forecasts with uncertainty propagation
#'
#' Generates stochastic forecast trajectories from a renewal equation while
#' propagating uncertainty in reproduction numbers (`rt`) and process noise.
#' Future incidence is simulated recursively, so uncertainty compounds over the
#' full forecast horizon.
#'
#' @param incidence Numeric vector of historical incidence counts.
#' @param serial_interval Numeric serial interval probability vector where the
#'   first element corresponds to a lag of one time step.
#' @param rt Reproduction-number input. Can be:
#'   * a scalar (constant across all simulations and forecast days),
#'   * a vector of length `horizon` (time-varying but shared across
#'     simulations), or
#'   * a matrix with `n_sims` rows and `horizon` columns (fully stochastic
#'     paths).
#' @param horizon Number of forecast days. If `NULL`, inferred from `rt` when
#'   `rt` is a vector or matrix. Defaults to 7 when `rt` is scalar.
#' @param n_sims Number of simulation paths when `rt` is scalar or vector.
#' @param seed Optional random seed.
#'
#' @return A tibble with one row per simulation/day containing `sim`, `day`,
#'   `rt`, `infectiousness`, `expected_incidence`, and simulated `incidence`.
#' @export
simulate_renewal_forecast <- function(
    incidence,
    serial_interval,
    rt = 1,
    horizon = NULL,
    n_sims = 1000L,
    seed = NULL
) {
  validate_nonnegative_numeric(incidence, "incidence")

  if (!is.numeric(horizon) && !is.null(horizon)) {
    rlang::abort("`horizon` must be NULL or a positive integer.")
  }

  if (is.null(horizon)) {
    if (is.matrix(rt)) {
      horizon <- ncol(rt)
    } else if (length(rt) > 1L) {
      horizon <- length(rt)
    } else {
      horizon <- 7L
    }
  }

  if (!is.numeric(horizon) || length(horizon) != 1L ||
      horizon < 1 || horizon != as.integer(horizon)) {
    rlang::abort("`horizon` must be a positive integer.")
  }

  w <- normalize_serial_interval(serial_interval)
  rt_mat <- build_rt_matrix(rt = rt, n_sims = n_sims, horizon = horizon)
  n_sims_eff <- nrow(rt_mat)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_hist <- length(incidence)
  sims <- vector("list", n_sims_eff)

  for (sim_i in seq_len(n_sims_eff)) {
    series <- c(incidence, rep(0, horizon))
    infectiousness <- numeric(horizon)
    expected <- numeric(horizon)
    simulated <- numeric(horizon)

    for (h in seq_len(horizon)) {
      t_idx <- n_hist + h
      max_lag <- min(length(w), t_idx - 1L)

      if (max_lag > 0) {
        lags <- seq_len(max_lag)
        lambda_t <- sum(series[t_idx - lags] * w[lags])
      } else {
        lambda_t <- 0
      }

      mu_t <- rt_mat[sim_i, h] * lambda_t
      y_t <- stats::rpois(1, lambda = mu_t)

      series[t_idx] <- y_t
      infectiousness[h] <- lambda_t
      expected[h] <- mu_t
      simulated[h] <- y_t
    }

    sims[[sim_i]] <- tibble::tibble(
      sim = sim_i,
      day = seq_len(horizon),
      rt = as.numeric(rt_mat[sim_i, ]),
      infectiousness = infectiousness,
      expected_incidence = expected,
      incidence = simulated
    )
  }

  dplyr::bind_rows(sims)
}

#' Simulate chain-based incidence forecasts with epichains
#'
#' Uses `epichains::simulate_chains()` to generate branching-process forecast
#' trajectories from a specified number of initial cases. This provides a
#' chain-based alternative to renewal-Poisson simulations.
#'
#' @param initial_cases Number of index cases at forecast origin.
#' @param rt Scalar reproduction number used as the Poisson offspring mean.
#' @param serial_interval Numeric serial interval probability vector used to
#'   sample discrete generation times.
#' @param horizon Forecast horizon (days).
#' @param n_sims Number of chain simulations.
#' @param stat_threshold Maximum simulated chain size used to cap explosions.
#' @param pop Susceptible population cap passed to `epichains`.
#' @param seed Optional random seed.
#'
#' @return A tibble with columns `sim`, `day`, and simulated `incidence`.
#' @export
simulate_epichains_forecast <- function(
    initial_cases,
    rt,
    serial_interval,
    horizon = 14L,
    n_sims = 500L,
    stat_threshold = 10000,
    pop = Inf,
    seed = NULL
) {
  if (!is.numeric(initial_cases) || length(initial_cases) != 1L ||
      initial_cases < 1 || initial_cases != as.integer(initial_cases)) {
    rlang::abort("`initial_cases` must be a positive integer.")
  }

  if (!is.numeric(rt) || length(rt) != 1L || !is.finite(rt) || rt < 0) {
    rlang::abort("`rt` must be a single non-negative finite numeric value.")
  }

  if (!is.numeric(horizon) || length(horizon) != 1L ||
      horizon < 1 || horizon != as.integer(horizon)) {
    rlang::abort("`horizon` must be a positive integer.")
  }

  if (!is.numeric(n_sims) || length(n_sims) != 1L ||
      n_sims < 1 || n_sims != as.integer(n_sims)) {
    rlang::abort("`n_sims` must be a positive integer.")
  }

  if (!is.numeric(stat_threshold) || length(stat_threshold) != 1L ||
      !is.finite(stat_threshold) || stat_threshold < 1) {
    rlang::abort("`stat_threshold` must be a single finite value >= 1.")
  }

  if (!is.numeric(pop) || length(pop) != 1L || pop <= 0) {
    rlang::abort("`pop` must be a single positive numeric value.")
  }

  w <- normalize_serial_interval(serial_interval)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  generation_time <- function(n) {
    sample(seq_along(w), size = n, replace = TRUE, prob = w)
  }

  sims <- vector("list", n_sims)

  for (sim_i in seq_len(n_sims)) {
    chains <- epichains::simulate_chains(
      n_chains = initial_cases,
      statistic = "size",
      offspring_dist = stats::rpois,
      lambda = rt,
      stat_threshold = stat_threshold,
      pop = pop,
      generation_time = generation_time,
      t0 = 0,
      tf = horizon + 1
    )

    if (!"time" %in% names(chains)) {
      rlang::abort("`epichains::simulate_chains()` did not return a `time` column.")
    }

    secondary <- chains[!is.na(chains$infector), , drop = FALSE]
    if (nrow(secondary) == 0L) {
      day_counts <- integer(horizon)
    } else {
      day_idx <- as.integer(ceiling(secondary$time))
      day_idx <- day_idx[day_idx >= 1L & day_idx <= horizon]
      day_counts <- integer(horizon)
      if (length(day_idx) > 0L) {
        tab <- table(day_idx)
        day_counts[as.integer(names(tab))] <- as.integer(tab)
      }
    }

    sims[[sim_i]] <- tibble::tibble(
      sim = sim_i,
      day = seq_len(horizon),
      incidence = as.numeric(day_counts)
    )
  }

  dplyr::bind_rows(sims)
}

#' Summarise predictive uncertainty from forecast draws
#'
#' @param forecast_draws Forecast draws as a tibble with columns `sim`, `day`,
#'   and `incidence`, or as a numeric matrix with rows = simulations,
#'   columns = forecast days.
#' @param probs Quantiles to report.
#' @param interval_level Level for the central prediction interval.
#'
#' @return A tibble with one row per forecast day and uncertainty summaries
#'   (`mean`, `median`, `sd`, interval bounds, and requested quantiles).
#' @export
summarise_forecast_uncertainty <- function(
    forecast_draws,
    probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
    interval_level = 0.9
) {
  draw_tbl <- coerce_forecast_draws(forecast_draws)

  if (!is.numeric(interval_level) || length(interval_level) != 1L ||
      !is.finite(interval_level) || interval_level <= 0 || interval_level >= 1) {
    rlang::abort("`interval_level` must be a single number in (0, 1).")
  }

  if (!is.numeric(probs) || any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) {
    rlang::abort("`probs` must be numeric probabilities in (0, 1).")
  }
  probs <- sort(unique(probs))

  alpha <- 1 - interval_level
  p_low <- alpha / 2
  p_high <- 1 - alpha / 2

  split_draws <- split(draw_tbl$incidence, draw_tbl$day)

  out <- purrr::imap_dfr(split_draws, function(x, day_chr) {
    q_vals <- stats::quantile(x, probs = probs, names = FALSE, type = 8)
    q_names <- paste0("q", formatC(probs * 100, format = "fg", digits = 6, flag = "#"))
    q_tbl <- as.list(stats::setNames(as.numeric(q_vals), q_names))

    tibble::tibble(
      day = as.integer(day_chr),
      mean = mean(x),
      median = stats::median(x),
      sd = stats::sd(x),
      lower = as.numeric(stats::quantile(x, p_low, names = FALSE, type = 8)),
      upper = as.numeric(stats::quantile(x, p_high, names = FALSE, type = 8)),
      interval_level = interval_level,
      !!!q_tbl
    )
  })

  dplyr::arrange(out, .data$day)
}

#' Score probabilistic count forecasts
#'
#' Computes point and distributional forecast scores for each horizon day,
#' including MAE, RMSE, interval coverage, WIS, and CRPS.
#'
#' @param observed Numeric vector of observed counts for the forecast horizon.
#' @param forecast_draws Forecast draws as a tibble with columns `sim`, `day`,
#'   and `incidence`, or as a numeric matrix with rows = simulations and
#'   columns = horizon days.
#' @param point Summary used for point predictions (`"median"` or `"mean"`).
#' @param interval_level Central prediction-interval level for coverage.
#' @param wis_levels Central interval levels used in the weighted interval
#'   score.
#'
#' @return A tibble with one row per day and forecast score columns.
#' @export
score_forecast_draws <- function(
    observed,
    forecast_draws,
    point = c("median", "mean"),
    interval_level = 0.9,
    wis_levels = c(0.5, 0.8, 0.95)
) {
  validate_nonnegative_numeric(observed, "observed")
  point <- rlang::arg_match(point)

  if (!is.numeric(interval_level) || length(interval_level) != 1L ||
      !is.finite(interval_level) || interval_level <= 0 || interval_level >= 1) {
    rlang::abort("`interval_level` must be a single number in (0, 1).")
  }

  if (!is.numeric(wis_levels) || any(!is.finite(wis_levels)) ||
      any(wis_levels <= 0 | wis_levels >= 1)) {
    rlang::abort("`wis_levels` must be numeric levels in (0, 1).")
  }
  wis_levels <- sort(unique(wis_levels))

  draw_tbl <- coerce_forecast_draws(forecast_draws)
  h <- length(observed)

  if (!all(seq_len(h) %in% draw_tbl$day)) {
    rlang::abort("`forecast_draws` must contain simulated draws for each day in `observed`.")
  }

  alpha <- 1 - interval_level
  p_low <- alpha / 2
  p_high <- 1 - alpha / 2

  split_draws <- split(draw_tbl$incidence, draw_tbl$day)

  score_tbl <- purrr::map_dfr(seq_len(h), function(day_i) {
    x <- split_draws[[as.character(day_i)]]
    y <- observed[[day_i]]

    point_pred <- if (point == "median") stats::median(x) else mean(x)
    lower <- as.numeric(stats::quantile(x, p_low, names = FALSE, type = 8))
    upper <- as.numeric(stats::quantile(x, p_high, names = FALSE, type = 8))

    abs_err <- abs(point_pred - y)
    sq_err <- (point_pred - y)^2

    tibble::tibble(
      day = day_i,
      observed = y,
      point_forecast = point_pred,
      abs_error = abs_err,
      sq_error = sq_err,
      mae = abs_err,
      rmse = sqrt(sq_err),
      lower = lower,
      upper = upper,
      interval_level = interval_level,
      covered = as.numeric(y >= lower && y <= upper),
      wis = weighted_interval_score(y = y, draws = x, levels = wis_levels),
      crps = sample_crps(y = y, draws = x)
    )
  })

  score_tbl
}

#' Rolling-origin backtesting for count forecasts
#'
#' Runs rolling-origin forecast evaluation by repeatedly refitting on expanding
#' windows and scoring out-of-sample forecast horizons. The default
#' `rt_method = "growth"` maps growth-rate uncertainty from
#' [fit_poisson_growth()] into `R_t` draws used by
#' [simulate_renewal_forecast()].
#'
#' @param data Data frame containing date and incidence columns.
#' @param date_col Name of date column in `data`.
#' @param count_col Name of incidence column in `data`.
#' @param initial_window Initial training-window length.
#' @param horizon Forecast horizon for each origin.
#' @param step Step size between forecast origins.
#' @param serial_interval Numeric serial interval probability vector.
#' @param rt_method Method for generating `R_t` forecasts (`"growth"` or
#'   `"fixed"`).
#' @param fixed_rt Scalar or length-`horizon` vector of fixed `R_t` values when
#'   `rt_method = "fixed"`.
#' @param n_sims Number of forecast simulations per origin.
#' @param mean_si Mean serial interval used to map growth-rate draws to `R_t`
#'   draws under `rt_method = "growth"`.
#' @param interval_level Central interval level for coverage in scoring.
#' @param wis_levels Levels used for WIS.
#' @param seed Optional random seed.
#'
#' @return A list with components:
#'   * `scores`: day-level scores across all origins,
#'   * `origin_summary`: origin-level aggregated metrics,
#'   * `overall`: global aggregated metrics,
#'   * `forecast_summary`: predictive summary quantiles by origin/day.
#' @export
rolling_origin_evaluate <- function(
    data,
    date_col = "date",
    count_col = "incidence",
    initial_window,
    horizon = 7L,
    step = 1L,
    serial_interval,
    rt_method = c("growth", "fixed"),
    fixed_rt = 1,
    n_sims = 500L,
    mean_si = 4.7,
    interval_level = 0.9,
    wis_levels = c(0.5, 0.8, 0.95),
    seed = NULL
) {
  rt_method <- rlang::arg_match(rt_method)

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

  if (!is.numeric(n_sims) || length(n_sims) != 1L ||
      n_sims < 1 || n_sims != as.integer(n_sims)) {
    rlang::abort("`n_sims` must be a positive integer.")
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

  y <- incid_tbl$incidence
  dates <- incid_tbl$date
  n <- length(y)

  if (initial_window + horizon > n) {
    rlang::abort("Not enough data for requested `initial_window` + `horizon`.")
  }

  origins <- seq(from = initial_window, to = n - horizon, by = step)

  score_list <- vector("list", length(origins))
  origin_summary_list <- vector("list", length(origins))
  forecast_summary_list <- vector("list", length(origins))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  for (i in seq_along(origins)) {
    origin_idx <- origins[[i]]
    train_counts <- y[seq_len(origin_idx)]
    obs <- y[seq.int(origin_idx + 1L, origin_idx + horizon)]

    rt_input <- switch(
      rt_method,
      growth = {
        train_df <- tibble::tibble(
          date = dates[seq_len(origin_idx)],
          incidence = train_counts
        )
        gfit <- fit_poisson_growth(train_df)
        coef_vec <- stats::coef(gfit$model)
        vc <- stats::vcov(gfit$model)

        slope_mean <- as.numeric(coef_vec[["day"]])
        slope_sd <- sqrt(as.numeric(vc["day", "day"]))

        slope_draws <- if (is.finite(slope_sd) && slope_sd > 0) {
          stats::rnorm(n_sims, mean = slope_mean, sd = slope_sd)
        } else {
          rep(slope_mean, n_sims)
        }

        rt_draws <- pmax(exp(slope_draws * mean_si), 0)
        matrix(rt_draws, nrow = n_sims, ncol = horizon)
      },
      fixed = {
        build_rt_matrix(rt = fixed_rt, n_sims = n_sims, horizon = horizon)
      }
    )

    draws <- simulate_renewal_forecast(
      incidence = train_counts,
      serial_interval = serial_interval,
      rt = rt_input,
      horizon = horizon,
      n_sims = n_sims
    )

    scores <- score_forecast_draws(
      observed = obs,
      forecast_draws = draws,
      point = "median",
      interval_level = interval_level,
      wis_levels = wis_levels
    ) |>
      dplyr::mutate(
        origin_index = origin_idx,
        origin_date = dates[[origin_idx]],
        target_date = dates[origin_idx + .data$day]
      )

    forecast_summary <- summarise_forecast_uncertainty(
      forecast_draws = draws,
      interval_level = interval_level
    ) |>
      dplyr::mutate(
        origin_index = origin_idx,
        origin_date = dates[[origin_idx]],
        target_date = dates[origin_idx + .data$day]
      )

    origin_summary <- scores |>
      dplyr::summarise(
        mae = mean(.data$abs_error),
        rmse = sqrt(mean(.data$sq_error)),
        coverage = mean(.data$covered),
        wis = mean(.data$wis),
        crps = mean(.data$crps)
      ) |>
      dplyr::mutate(
        origin_index = origin_idx,
        origin_date = dates[[origin_idx]],
        .before = 1
      )

    score_list[[i]] <- scores
    origin_summary_list[[i]] <- origin_summary
    forecast_summary_list[[i]] <- forecast_summary
  }

  all_scores <- dplyr::bind_rows(score_list)
  origin_summary <- dplyr::bind_rows(origin_summary_list)
  forecast_summary <- dplyr::bind_rows(forecast_summary_list)

  overall <- all_scores |>
    dplyr::summarise(
      mae = mean(.data$abs_error),
      rmse = sqrt(mean(.data$sq_error)),
      coverage = mean(.data$covered),
      wis = mean(.data$wis),
      crps = mean(.data$crps),
      n_origins = dplyr::n_distinct(.data$origin_index),
      n_forecasts = dplyr::n()
    )

  list(
    scores = all_scores,
    origin_summary = origin_summary,
    overall = overall,
    forecast_summary = forecast_summary
  )
}
