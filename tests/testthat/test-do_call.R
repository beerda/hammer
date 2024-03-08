test_that("do_call ignoring unused", {
    f <- function(a, b) {
        paste0("a=", a, " b=", b)
    }

    expect_equal(do_call(f, .args = list(a=1, b=2)),
                 "a=1 b=2")
    expect_equal(do_call(f, a=1, b=2),
                 "a=1 b=2")
    expect_equal(do_call(f, .args = list(a=1, b=2, c=3)),
                 "a=1 b=2")
    expect_equal(do_call(f, a=1, b=2, c=3),
                 "a=1 b=2")

    expect_error(do_call(f,
                         .args = list(a=1, b=2, c=3),
                         .ignore_unused = FALSE))
})


test_that("do_call handling dots", {
    f <- function(a, b, ...) {
        dots <- list(...)
        if (length(dots) <= 0)
            dots <- ""
        else
            dots <- paste0(" ", names(dots), "=", dots, collapse = "")

        paste0("a=", a, " b=", b, dots)
    }

    expect_equal(do_call(f, .args = list(a=1, b=2)),
                 "a=1 b=2")
    expect_equal(do_call(f, a=1, b=2),
                 "a=1 b=2")
    expect_equal(do_call(f, .args = list(a=1, b=2, c=3)),
                 "a=1 b=2 c=3")
    expect_equal(do_call(f, a=1, b=2, c=3),
                 "a=1 b=2 c=3")
    expect_equal(do_call(f, .args = list(a=1, b=2, c=3, d=4)),
                 "a=1 b=2 c=3 d=4")
    expect_equal(do_call(f, a=1, b=2, c=3, d=4),
                 "a=1 b=2 c=3 d=4")
})
