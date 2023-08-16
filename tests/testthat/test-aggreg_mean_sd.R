test_that("format_mean_sd", {
    expect_equal(aggreg_mean_sd(1:9, digits = 2),
                 "5.00 ± 2.74")
    expect_equal(aggreg_mean_sd(NA_real_, digits = 2),
                 "NA ± NA")
})
