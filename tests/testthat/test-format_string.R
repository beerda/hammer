test_that("format_string", {
    str <- rep(letters, 100)
    str <- paste0(str, collapse="")

    expect_equal(format_string(c(str, "a", str)),
                 c(str, "a", str))
    expect_equal(format_string(c(str, "a", "abcde", str), max_width = 5),
                 c("abcde...", "a", "abcde", "abcde..."))

})
