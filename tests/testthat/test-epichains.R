test_that("simulate_epichains_paths returns simulation and summary outputs", {
  sims <- simulate_epichains_paths(
    rt = c(0.8, 0.9, 1.0, 1.1),
    n_sims = 20,
    horizon = 5,
    offspring_dist = "poisson",
    seed = 5
  )

  expect_true(is.list(sims))
  expect_true(all(c("simulations", "summary") %in% names(sims)))
  expect_equal(length(unique(sims$simulations$sim)), 20)
  expect_equal(nrow(sims$summary), 6)
  expect_true(all(c("day_ahead", "mean_incidence", "q05", "q95") %in% names(sims$summary)))
})
