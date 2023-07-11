test_that("format_interval", {
  expect_equal(format_interval(1:3, 6:8, sep = " -- ", digits = 0),
               c(paste(1:3, "--", 6:8)))
  expect_equal(format_interval(1:2, sep = " -- ", digits = 2),
               "1.00 -- 2.00")
})
