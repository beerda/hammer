test_that("aggreg_median_iqr", {
    expect_equal(aggreg_median_iqr(1:9, digits = 2),
                 "5.00 (3.00 -- 7.00)")
    expect_equal(aggreg_median_iqr(NA_real_, digits = 2),
                 "NA (NA -- NA)")
})
