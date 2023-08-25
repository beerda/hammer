test_that("test t", {
    set.seed(333)
    n <- 30
    a <- rnorm(n, 0, 1)
    b <- rnorm(n, 5, 3)
    x <- c(a, b)
    g <- factor(c(rep("a", n), rep("b", n)))

    expect_equal(test_t(x, g),
                 t.test(a, b)$p.value)
})
