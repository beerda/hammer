test_that("format_pvalue", {
  expect_equal(format_pvalue(0.0012, digits = 4, thresh = 0.05),
               "0.0012")
  expect_equal(format_pvalue(0.0001, digits = 4, thresh = 0.05),
               "0.0001")
  expect_equal(format_pvalue(0.00009, digits = 4, thresh = 0.05),
               "<0.0001")
  expect_equal(format_pvalue(0.0012, digits = 4, thresh = 0.0001),
               "NS")
  expect_equal(format_pvalue(NA_real_, digits = 4, thresh = 0.0001),
               "NA")

  expect_equal(format_pvalue(0.0012, digits = 4, thresh = 0.05, varname = 'p'),
               "p=0.0012")
  expect_equal(format_pvalue(0.0001, digits = 4, thresh = 0.05, varname = 'p'),
               "p=0.0001")
  expect_equal(format_pvalue(0.00009, digits = 4, thresh = 0.05, varname = 'p'),
               "p<0.0001")
  expect_equal(format_pvalue(0.0012, digits = 4, thresh = 0.0001, varname = 'p'),
               "p=NS")
  expect_equal(format_pvalue(NA_real_, digits = 4, thresh = 0.0001, varname = 'p'),
               "p=NA")

  expect_equal(format_pvalue(c(NA, 0.2, 0.0012), digits = 4, thresh = 0.05),
               c("NA", "NS", "0.0012"))
})
