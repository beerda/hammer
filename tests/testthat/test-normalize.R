test_that("normalize vector", {
    res <- normalize(-1:1, normalize = FALSE)
    expect_equal(as.vector(res), -1:1)
    expect_equal(attr(res, "scaled:center"), 0)
    expect_equal(attr(res, "scaled:scale"), 1)
    expect_true(is.null(attr(res, "normalized:method")))

    res <- normalize(-1:1, normalize = TRUE)
    expect_equal(as.vector(res), -1:1)
    expect_equal(attr(res, "scaled:center"), 0)
    expect_equal(attr(res, "scaled:scale"), 1)
    expect_true(!is.null(attr(res, "normalized:method")))
})


test_that("normalize data frame", {
    dat <- data.frame(a = -1:1,
                    b = letters[1:3],
                    c = 2 * (-1:1),
                    d = 3 * (-1:1))

    res <- normalize(dat, normalize = FALSE)
    expect_equal(colnames(res), c("a", "b", "c", "d"))
    expect_equal(as.vector(res$a), -1:1)
    expect_equal(attr(res$a, "scaled:center"), 0)
    expect_equal(attr(res$a, "scaled:scale"), 1)
    expect_true(is.null(attr(res$a, "normalized:method")))

    expect_equal(res$b, letters[1:3])
    expect_true(is.null(attr(res$b, "normalized:method")))

    expect_equal(as.vector(res$c), -1:1)
    expect_equal(attr(res$c, "scaled:center"), 0)
    expect_equal(attr(res$c, "scaled:scale"), 2)
    expect_true(is.null(attr(res$c, "normalized:method")))

    expect_equal(as.vector(res$d), -1:1)
    expect_equal(attr(res$d, "scaled:center"), 0)
    expect_equal(attr(res$d, "scaled:scale"), 3)
    expect_true(is.null(attr(res$d, "normalized:method")))


    res <- normalize(dat)
    expect_equal(colnames(res), c("a", "b", "c", "d"))
    expect_equal(as.vector(res$a), -1:1)
    expect_true(!is.null(attr(res$a, "normalized:method")))

    expect_equal(res$b, letters[1:3])
    expect_true(is.null(attr(res$b, "normalized:method")))

    expect_equal(as.vector(res$c), -1:1)
    expect_true(!is.null(attr(res$c, "normalized:method")))

    expect_equal(as.vector(res$d), -1:1)
    expect_true(!is.null(attr(res$d, "normalized:method")))


    res <- normalize(dat, what = a:c)
    expect_equal(colnames(res), c("a", "b", "c", "d"))
    expect_equal(as.vector(res$a), -1:1)
    expect_true(!is.null(attr(res$a, "normalized:method")))

    expect_equal(res$b, letters[1:3])
    expect_true(is.null(attr(res$b, "normalized:method")))

    expect_equal(as.vector(res$c), -1:1)
    expect_true(!is.null(attr(res$c, "normalized:method")))

    expect_equal(as.vector(res$d), 3 * (-1:1))
    expect_true(is.null(attr(res$d, "normalized:method")))
})
