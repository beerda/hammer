test_that("foreach_col_by_class", {
    d <- data.frame(a1 = 1:5,
                    a2 = 101:105,
                    b1 = letters[1:5],
                    b2 = LETTERS[1:5])

    handlers <- list(factor = function(...) "factor")
    expect_equal(foreach_col_by_class(d, handlers),
                 list(a1 = NULL, a2 = NULL, b1 = NULL, b2 = NULL))

    handlers <- list(factor = function(...) "factor")
    expect_equal(foreach_col_by_class(d, handlers, .fail = "x"),
                 list(a1 = "x", a2 = "x", b1 = "x", b2 = "x"))

    handlers <- list(factor = function(...) "factor",
                     integer = function(...) "integer")
    expect_equal(foreach_col_by_class(d, handlers),
                 list(a1 = "integer", a2 = "integer", b1 = NULL, b2 = NULL))

    handlers <- list(factor = function(...) "factor",
                     integer = function(...) "integer",
                     character = function(...) "character")
    expect_equal(foreach_col_by_class(d, handlers),
                 list(a1 = "integer", a2 = "integer", b1 = "character", b2 = "character"))
})


test_that("foreach_col_by_class multiple classes", {
    d <- data.frame(a1 = 1:5,
                    a2 = 101:105,
                    b1 = letters[1:5],
                    b2 = LETTERS[1:5])

    class(d$a1) <- c("x", "y")
    class(d$a2) <- c("x")
    class(d$b1) <- c("y")
    class(d$b2) <- c("y", "z")

    handlers <- list(x = function(...) "x",
                     y = function(...) "y",
                     z = function(...) "z")
    expect_equal(foreach_col_by_class(d, handlers),
                 list(a1 = "x", a2 = "x", b1 = "y", b2 = "y"))
})
