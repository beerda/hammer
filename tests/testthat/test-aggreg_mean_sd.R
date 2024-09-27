test_that("format_mean_sd", {
    expect_equal(aggreg_mean_sd(data.frame(x = 1:9), "x", digits = 2),
                 "5.00 ± 2.74")
    expect_equal(aggreg_mean_sd(data.frame(x = NA_real_), "x", digits = 2),
                 "NA ± NA")
})
