test_that("aggreg_median_iqr", {
    expect_equal(aggreg_median_iqr(data.frame(x = 1:9), "x", digits = 2),
                 "5.00 (3.00 -- 7.00)")
    expect_equal(aggreg_median_iqr(data.frame(x = NA_real_), "x", digits = 2),
                 "NA (NA -- NA)")
})
