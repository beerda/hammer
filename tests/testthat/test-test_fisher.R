test_that("test_fisher", {
    set.seed(333)
    n <- 30
    a <- sample(c("1", "2"), n, replace = TRUE)
    b <- sample(c("3", "4"), n, replace = TRUE)

    expect_equal(test_fisher(factor(a), factor(b)),
                 fisher.test(table(a, b))$p.value)
})
