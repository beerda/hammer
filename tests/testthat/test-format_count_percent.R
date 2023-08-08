test_that("format_count_percent", {
  expect_equal(format_count_percent(p = 5, n = 10, digits = 2),
               "5 (50.00 %)")
  expect_equal(format_count_percent(p = 3:5, n = 10, digits = 0),
               c("3 (30 %)", "4 (40 %)", "5 (50 %)"))
  expect_equal(format_count_percent(p = c(5, NA, 7), n = 10, digits = 2, ignore_na = FALSE),
               c("5 (50.00 %)", "NA (NA %)", "7 (70.00 %)"))
  expect_equal(format_count_percent(p = c(5, NA, 7), n = 10, digits = 2, ignore_na = TRUE),
               c("5 (50.00 %)", "", "7 (70.00 %)"))
  expect_equal(format_count_percent(p = c(5, NA, 7), n = 10, digits = 2, ignore_na = "--"),
               c("5 (50.00 %)", "--", "7 (70.00 %)"))
})
