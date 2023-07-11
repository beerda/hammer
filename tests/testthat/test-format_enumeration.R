test_that("format_enumeration", {
  expect_equal(format_enumeration(letters[1:3], sep = "; ", max = 5, dots = ".."),
               "a; b; c")
  expect_equal(format_enumeration(letters[1:3], sep = "; ", max = 3, dots = ".."),
               "a; b; c")
  expect_equal(format_enumeration(letters[1:3], sep = "; ", max = 2, dots = ".."),
               "a; b; ..")
  expect_equal(format_enumeration(NULL),
               "")
})
