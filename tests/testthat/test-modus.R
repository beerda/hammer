test_that("modus", {
    expect_equal(modus(NA), NA)
    expect_equal(modus(c(NA, NA)), NA)
    expect_equal(modus(c(NA, NA), na_rm = TRUE), NA)

    expect_equal(modus(c(1, 2, 2, 3, 4)), 2)
    expect_equal(modus(c(1, 2, 3, 4)), 1)

    expect_equal(modus(c("a", "b", "c", "a", "c")), "a")
    expect_equal(modus(c("a", "c", "c", "a", "c")), "c")
    expect_equal(modus(c("a", "c", "c", NA, NA, NA), na_rm = T), "c")
    expect_equal(modus(c("a", "c", "c", NA, NA, NA), na_rm = F), NA_character_)

    expect_equal(modus(c(T, T, F, F, F)), F)
})
