test_that("test_wilcox", {
    set.seed(333)
    n <- 30
    a <- rnorm(n, 0, 1)
    b <- rnorm(n, 5, 3)
    x <- c(a, b)
    g <- factor(c(rep("a", n), rep("b", n)))

    expect_equal(test_wilcox(x, g),
                 wilcox.test(a, b)$p.value)
})
