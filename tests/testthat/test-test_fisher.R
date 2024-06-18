test_that("test_fisher", {
    set.seed(333)
    n <- 30
    a <- sample(c("1", "2"), n, replace = TRUE)
    b <- sample(c("3", "4"), n, replace = TRUE)
    err <- rep("X", n)

    expect_equal(test_fisher(factor(a), factor(b)),
                 fisher.test(table(a, b))$p.value)

    expect_equal(test_fisher(factor(a), factor(err)), NA_real_)
    expect_equal(test_fisher(factor(err), factor(a)), NA_real_)
})
