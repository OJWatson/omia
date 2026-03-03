test_that("forecast_poisson_growth returns forecast summaries and draw matrices", {
  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:13,
    incidence = c(1, 1, 2, 2, 3, 4, 5, 6, 7, 7, 8, 9, 10, 12)
  )

  out <- forecast_poisson_growth(
    data = x,
    horizon = 5,
    level = 0.9,
    n_draws = 200,
    seed = 123
  )

  expect_true(is.list(out))
  expect_true(all(c("forecast", "draws", "expected_draws", "fit") %in% names(out)))
  expect_equal(nrow(out$forecast), 5)
  expect_equal(dim(out$draws), c(200, 5))
  expect_true(all(c("date", "day_ahead", "median_forecast", "lower", "upper") %in% names(out$forecast)))
})

test_that("evaluate_rolling_origin_forecast returns requested metrics", {
  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:24,
    incidence = c(1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 7, 7, 8, 9, 10, 10, 11, 12, 13, 14, 14, 15, 16, 17, 18)
  )

  out <- evaluate_rolling_origin_forecast(
    data = x,
    initial_window = 14,
    horizon = 3,
    step = 2,
    n_draws = 200,
    seed = 8
  )

  expect_true(is.list(out))
  expect_true(all(c("scores", "metrics_overall", "metrics_by_horizon") %in% names(out)))
  expect_equal(nrow(out$metrics_overall), 1)
  expect_true(all(c("mae", "rmse", "interval_coverage", "wis", "crps") %in% names(out$metrics_overall)))
  expect_equal(nrow(out$metrics_by_horizon), 3)
})
