test_that("prep_incidence_data standardizes and fills dates", {
  x <- tibble::tibble(
    d = as.Date("2024-01-01") + c(0, 2),
    n = c(1, 3)
  )

  out <- prep_incidence_data(x, date_col = "d", count_col = "n", complete_dates = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3)
  expect_equal(out$incidence, c(1, 0, 3))
})

test_that("prep_incidence_data rejects negative incidence", {
  x <- tibble::tibble(date = as.Date("2024-01-01"), incidence = -1)
  expect_error(prep_incidence_data(x), "non-negative")
})
