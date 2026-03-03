test_that("renewal_expected_incidence returns expected columns", {
  out <- renewal_expected_incidence(
    incidence = c(1, 2, 3, 4),
    rt = 1.1,
    serial_interval = c(0.5, 0.5)
  )

  expect_true(all(c("day", "infectiousness", "rt", "expected_incidence") %in% names(out)))
  expect_equal(nrow(out), 4)
})

test_that("simulate_poisson_process has reproducible seed", {
  x1 <- simulate_poisson_process(c(1, 2, 3), n_sims = 2, seed = 1)
  x2 <- simulate_poisson_process(c(1, 2, 3), n_sims = 2, seed = 1)

  expect_equal(x1$incidence, x2$incidence)
})
