# Build cat_ba_week1 dataset

counts <- c(1, 0, 1, 4, 1, 2, 0, 0, 3, 2, 4, 1, 5, 1, 3, 4, 4, 5, 9, 3, 8, 6, 12)

cat_ba_week1 <- tibble::tibble(
  day = seq_along(counts),
  date = as.Date("2024-01-01") + (seq_along(counts) - 1),
  incidence = counts
)

save(cat_ba_week1, file = "data/cat_ba_week1.rda", compress = "bzip2")
