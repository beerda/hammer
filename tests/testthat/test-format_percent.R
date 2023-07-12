test_that("format_percent", {
  expect_equal(format_percent(c(0.3, -2.24689, NA), digits = 2, na = "N/A"),
               c("30.00 %", "-224.69 %", "N/A %"))
})
