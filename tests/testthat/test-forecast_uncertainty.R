test_that("simulate_renewal_forecast is reproducible with a fixed seed", {
  draws1 <- simulate_renewal_forecast(
    incidence = c(1, 2, 3, 4),
    serial_interval = c(0.5, 0.3, 0.2),
    rt = 1.1,
    horizon = 5,
    n_sims = 20,
    seed = 123
  )

  draws2 <- simulate_renewal_forecast(
    incidence = c(1, 2, 3, 4),
    serial_interval = c(0.5, 0.3, 0.2),
    rt = 1.1,
    horizon = 5,
    n_sims = 20,
    seed = 123
  )

  expect_equal(draws1$incidence, draws2$incidence)
  expect_true(all(c("sim", "day", "rt", "infectiousness", "expected_incidence", "incidence") %in% names(draws1)))
})

test_that("summarise_forecast_uncertainty returns day-level distribution summaries", {
  draws <- simulate_renewal_forecast(
    incidence = c(1, 2, 3, 4),
    serial_interval = c(0.6, 0.4),
    rt = 1.05,
    horizon = 4,
    n_sims = 40,
    seed = 11
  )

  out <- summarise_forecast_uncertainty(draws, interval_level = 0.8)

  expect_equal(nrow(out), 4)
  expect_true(all(c("day", "mean", "median", "sd", "lower", "upper", "interval_level") %in% names(out)))
  expect_true(all(out$upper >= out$lower))
})

test_that("score_forecast_draws computes MAE/RMSE/coverage/WIS/CRPS", {
  draws <- simulate_renewal_forecast(
    incidence = c(1, 1, 2, 3, 4),
    serial_interval = c(0.5, 0.3, 0.2),
    rt = 1.0,
    horizon = 3,
    n_sims = 100,
    seed = 202
  )

  observed <- c(3, 2, 4)
  scores <- score_forecast_draws(observed, draws)

  expect_equal(nrow(scores), length(observed))
  expect_true(all(c("mae", "rmse", "covered", "wis", "crps") %in% names(scores)))
  expect_true(all(scores$wis >= 0))
  expect_true(all(scores$crps >= 0))
})

test_that("rolling_origin_evaluate returns coherent backtest outputs", {
  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:19,
    incidence = c(1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 7, 7, 8, 9, 10, 10, 11, 12, 12, 13)
  )

  out <- rolling_origin_evaluate(
    data = x,
    initial_window = 12,
    horizon = 3,
    step = 2,
    serial_interval = c(0.5, 0.3, 0.2),
    n_sims = 80,
    seed = 10
  )

  expect_true(is.list(out))
  expect_true(all(c("scores", "origin_summary", "overall", "forecast_summary") %in% names(out)))
  expect_gt(nrow(out$scores), 0)
  expect_gt(nrow(out$origin_summary), 0)
  expect_equal(nrow(out$overall), 1)
  expect_true(all(c("mae", "rmse", "coverage", "wis", "crps") %in% names(out$overall)))
})

test_that("simulate_epichains_forecast returns complete day grid", {
  sims <- simulate_epichains_forecast(
    initial_cases = 2,
    rt = 0.9,
    serial_interval = c(0.5, 0.3, 0.2),
    horizon = 5,
    n_sims = 15,
    seed = 3
  )

  expect_equal(nrow(sims), 15 * 5)
  expect_true(all(c("sim", "day", "incidence") %in% names(sims)))
  expect_true(all(sims$incidence >= 0))
})

test_that("fit_poisson_growth can return uncertainty draws", {
  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:9,
    incidence = c(1, 1, 2, 2, 3, 4, 4, 5, 6, 7)
  )

  out <- fit_poisson_growth(x, n_draws = 50, seed = 7)

  expect_true("implied_rt" %in% names(out$estimates))
  expect_equal(nrow(out$uncertainty_draws), 50)
  expect_true(all(c("growth_rate", "doubling_time", "implied_rt") %in% names(out$uncertainty_draws)))
})
