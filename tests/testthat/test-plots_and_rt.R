test_that("plot functions return ggplot objects", {
  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:4,
    incidence = c(1, 2, 1, 3, 2)
  )

  p1 <- plot_epicurve(x)
  expect_s3_class(p1, "ggplot")

  rt <- tibble::tibble(
    date_end = as.Date("2024-01-01") + 1:4,
    mean_r = c(1.1, 0.9, 1.2, 1.0),
    lower_95 = c(0.8, 0.7, 0.9, 0.8),
    upper_95 = c(1.4, 1.1, 1.5, 1.2)
  )

  p2 <- plot_rt_estimates(rt)
  expect_s3_class(p2, "ggplot")
})

test_that("estimate_rt_epiestim handles missing package gracefully", {
  if (rlang::is_installed("EpiEstim")) {
    testthat::skip("EpiEstim is installed; missing-package behavior not tested.")
  }

  x <- tibble::tibble(
    date = as.Date("2024-01-01") + 0:9,
    incidence = c(1, 2, 1, 3, 2, 2, 4, 3, 2, 1)
  )

  expect_error(estimate_rt_epiestim(x), "EpiEstim")
})
