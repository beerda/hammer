test_that("test_chisq", {
    set.seed(333)
    n <- 30
    a <- sample(c("1", "2"), n, replace = TRUE)
    b <- sample(c("3", "4"), n, replace = TRUE)
    err <- rep("X", n)

    expect_equal(test_chisq(factor(a), factor(b)),
                 chisq.test(table(a, b))$p.value)

    expect_equal(test_chisq(factor(a), factor(err)), NA_real_)
    expect_equal(test_chisq(factor(err), factor(a)), NA_real_)
})
