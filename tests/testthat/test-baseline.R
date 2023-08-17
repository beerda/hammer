test_that("baseline test_numeric n/all", {
    d <- data.frame(p = c(1, 1, 1, 3, 1, 1, 1, 4),
                    q = c(1.7328354, -1.3542925, 1.2344112, -0.1440034, -0.6688967, -0.4355036, -0.5692527, -0.4613416),
                    g = factor(c("x", "x", "x", "x", "y", "y", "y", "y")))
    d <- group_by(d, g)
    res <- baseline(d,
                    .n = TRUE,
                    .all = TRUE,
                    .bullet = " * ",
                    .numeric_stat = "test")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "x", "y", "all"))
    expect_equal(res$name,
                 c("N", "p", "q"))
    expect_equal(res$x,
                 c("4", "1.00 (1.00 -- 1.50)", "0.37 ± 1.40"))
    expect_equal(res$y,
                 c("4", "1.00 (1.00 -- 1.75)", "-0.53 ± 0.11"))
    expect_equal(res$all,
                 c("8", "1.00 (1.00 -- 1.50)", "-0.08 ± 1.04"))
})


test_that("baseline parametric n/all", {
    d <- data.frame(p = 1:5,
                    q = factor(letters[1:5 %% 2 + 1]),
                    g = factor(c("x", "x", "x", "y", "y")))
    d <- group_by(d, g)
    res <- baseline(d,
                    .n = TRUE,
                    .all = TRUE,
                    .bullet = " * ",
                    .numeric_stat = "parametric")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "x", "y", "all"))
    expect_equal(res$name,
                 c("N", "p", "q:", " * a", " * b"))
    expect_equal(res$x,
                 c("3", "2.00 ± 1.00", "", "1 (33.33 %)", "2 (66.67 %)"))
    expect_equal(res$y,
                 c("2", "4.50 ± 0.71", "", "1 (50.00 %)", "1 (50.00 %)"))
    expect_equal(res$all,
                 c("5", "3.00 ± 1.58", "", "2 (40.00 %)", "3 (60.00 %)"))
})


test_that("baseline robust n/all", {
    d <- data.frame(p = 1:5,
                    q = factor(letters[1:5 %% 2 + 1]),
                    g = factor(c("x", "x", "x", "y", "y")))
    d <- group_by(d, g)
    res <- baseline(d,
                    .n = TRUE,
                    .all = TRUE,
                    .bullet = " * ",
                    .numeric_stat = "robust")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "x", "y", "all"))
    expect_equal(res$name,
                 c("N", "p", "q:", " * a", " * b"))
    expect_equal(res$x,
                 c("3", "2.00 (1.50 -- 2.50)", "", "1 (33.33 %)", "2 (66.67 %)"))
    expect_equal(res$y,
                 c("2", "4.50 (4.25 -- 4.75)", "", "1 (50.00 %)", "1 (50.00 %)"))
    expect_equal(res$all,
                 c("5", "3.00 (2.00 -- 4.00)", "", "2 (40.00 %)", "3 (60.00 %)"))
})


test_that("baseline robust !n/all", {
    d <- data.frame(p = 1:5,
                    q = factor(letters[1:5 %% 2 + 1]),
                    g = factor(c("x", "x", "x", "y", "y")))
    d <- group_by(d, g)
    res <- baseline(d,
                    .n = FALSE,
                    .all = TRUE,
                    .bullet = " * ",
                    .numeric_stat = "robust")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "x", "y", "all"))
    expect_equal(res$name,
                 c("p", "q:", " * a", " * b"))
    expect_equal(res$x,
                 c("2.00 (1.50 -- 2.50)", "", "1 (33.33 %)", "2 (66.67 %)"))
    expect_equal(res$y,
                 c("4.50 (4.25 -- 4.75)", "", "1 (50.00 %)", "1 (50.00 %)"))
    expect_equal(res$all,
                 c("3.00 (2.00 -- 4.00)", "", "2 (40.00 %)", "3 (60.00 %)"))
})


test_that("baseline robust n/!all", {
    d <- data.frame(p = 1:5,
                    q = factor(letters[1:5 %% 2 + 1]),
                    g = factor(c("x", "x", "x", "y", "y")))
    d <- group_by(d, g)
    res <- baseline(d,
                    .n = TRUE,
                    .all = FALSE,
                    .bullet = " * ",
                    .numeric_stat = "robust")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "x", "y"))
    expect_equal(res$name,
                 c("N", "p", "q:", " * a", " * b"))
    expect_equal(res$x,
                 c("3", "2.00 (1.50 -- 2.50)", "", "1 (33.33 %)", "2 (66.67 %)"))
    expect_equal(res$y,
                 c("2", "4.50 (4.25 -- 4.75)", "", "1 (50.00 %)", "1 (50.00 %)"))
})