test_that("format_number", {
  expect_equal(format_number(c(-1.342, 55.1284, NA, 0.1, 0),
                             digits = 2, na = "N/A"),
               c("-1.34", "55.13", "N/A", "0.10", "0.00"))
})
