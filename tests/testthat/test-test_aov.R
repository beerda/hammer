test_that("test_aov", {
    set.seed(333)
    n <- 30
    a <- rnorm(n, 0, 1)
    b <- rnorm(n, 5, 3)
    x <- c(a, b)
    g <- factor(c(rep("a", n), rep("b", n)))

    expect_equal(test_aov(x, g),
                 3.61111e-14)
})

test_that("test_aov with confounders", {
    set.seed(333)
    n <- 30
    a <- rnorm(n, 0, 1)
    b <- rnorm(n, 5, 3)

    c <- rnorm(2*n, 50, 3)
    d <- rnorm(2*n, 50, 3)
    e <- rnorm(2*n, 50, 3)

    x <- c(a, b)
    g <- factor(c(rep("a", n), rep("b", n)))
    confounders <- list(x = c, g = d, e = e)

    expect_equal(test_aov(x, g, confounders),
                 2.168191e-14)
})
