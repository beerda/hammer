test_that("unlist first", {
    expect_equal(unlist_first(list(1:5, letters[1:5], NULL, NA)),
                 c("1", "a", NA))

    expect_equal(unlist_first(list(1:5, 11:15, 3, 100:200)),
                 c(1, 11, 3, 100))
})
