test_that("format_bars", {
  expect_equal(format_bars(p = 0, n = 10, width = 5),
               "-----")
  expect_equal(format_bars(p = 1, n = 10, width = 5),
               "I----")
  expect_equal(format_bars(p = 2, n = 10, width = 5),
               "I----")
  expect_equal(format_bars(p = 3, n = 10, width = 5),
               "II---")
  expect_equal(format_bars(p = 4, n = 10, width = 5),
               "II---")
  expect_equal(format_bars(p = 5, n = 10, width = 5),
               "II---")
  expect_equal(format_bars(p = 6, n = 10, width = 5),
               "III--")
  expect_equal(format_bars(p = 7, n = 10, width = 5),
               "IIII-")
  expect_equal(format_bars(p = 8, n = 10, width = 5),
               "IIII-")
  expect_equal(format_bars(p = 9, n = 10, width = 5),
               "IIII-")
  expect_equal(format_bars(p = 10, n = 10, width = 5),
               "IIIII")
})
