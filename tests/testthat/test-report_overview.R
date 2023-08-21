test_that("report_overview", {
    d <- data.frame(a = 1:5,
                    b = c(1, 2, NA, NA, NA),
                    c = rep(NA, 5),
                    d = rep(5, 5))

    res <- report_overview(d)
    expect_equal(res$name,
                 c("rows:", "\u2022 fully specified", "\u2022 partially N/A", "\u2022 fully N/A",
                   "columns:", "\u2022 constant", "\u2022 fully specified", "\u2022 partially N/A", "\u2022 fully N/A"))
    expect_equal(res$value,
                 c("5 (100.00 %)", "0 (0.00 %)", "5 (100.00 %)", "0 (0.00 %)",
                   "4 (100.00 %)", "2 (50.00 %)", "2 (50.00 %)", "1 (25.00 %)", "1 (25.00 %)"))
    expect_equal(res$details,
                 c("", "", "1, 2, 3, 4, 5", "", "", "c, d", "a, d", "b", "c"))
})
