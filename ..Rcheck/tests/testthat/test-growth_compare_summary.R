test_that("fit_poisson_growth returns model and estimates", {
  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:9,
    incidence = c(1, 1, 2, 2, 3, 4, 4, 5, 6, 7)
  )

  out <- fit_poisson_growth(x)

  expect_true(is.list(out))
  expect_true(inherits(out$model, "glm"))
  expect_true(all(c("growth_rate", "doubling_time") %in% names(out$estimates)))
})

test_that("compare_incidence_models computes AIC and BIC", {
  d <- tibble::tibble(y = c(1, 2, 3, 4), x = 1:4)
  m1 <- stats::glm(y ~ x, data = d, family = stats::poisson())
  m2 <- stats::glm(y ~ 1, data = d, family = stats::poisson())

  out <- compare_incidence_models(list(full = m1, null = m2))

  expect_true(all(c("model", "aic", "bic") %in% names(out)))
  expect_equal(nrow(out), 2)
})

test_that("outbreak_consensus_summary returns one row", {
  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:4,
    incidence = c(1, 3, 2, 4, 1)
  )
  g <- fit_poisson_growth(x)

  out <- outbreak_consensus_summary(
    incidence_data = x,
    rt_estimates = tibble::tibble(),
    growth_fit = g
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
})
