test_that("cat_ba_week1 dataset is available and valid", {
  data("cat_ba_week1", package = "omia")

  expect_true(exists("cat_ba_week1"))
  expect_equal(nrow(cat_ba_week1), 23)
  expect_equal(sum(cat_ba_week1$incidence), 79)
})
