test_that("baseline test_numeric n/all", {
    d <- data.frame(p = c(1, 1, 1, 3, 1, 1, 1, 4),
                    q = c(1.7328354, -1.3542925, 1.2344112, -0.1440034, -0.6688967, -0.4355036, -0.5692527, -0.4613416),
                    g = factor(c("x", "x", "x", "x", "y", "y", "y", "y")))
    d <- group_by(d, g)
    res <- baseline(d,
                    .n = TRUE,
                    .all = TRUE,
                    .test = FALSE,
                    .bullet = " * ",
                    .numeric_aggreg = create_numeric_aggreg("test"),
                    exact = FALSE)

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
                    .test = FALSE,
                    .bullet = " * ",
                    .numeric_aggreg = create_numeric_aggreg("parametric"))

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
                    .test = FALSE,
                    .bullet = " * ",
                    .numeric_aggreg = create_numeric_aggreg("robust"))

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
                    .test = FALSE,
                    .bullet = " * ",
                    .numeric_aggreg = create_numeric_aggreg("robust"))

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
                    .test = FALSE,
                    .bullet = " * ",
                    .numeric_aggreg = create_numeric_aggreg("robust"))

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


test_that("baseline pvalues", {
    set.seed(333)
    n <- 30
    d <- data.frame(p = c(rnorm(n, 1, 3), rnorm(n, 10, 5)),
                    q = rnorm(2 * n),
                    g = factor(c(rep("a", n), rep("b", n))))
    d <- group_by(d, g)
    res <- baseline(d,
                    .n = FALSE,
                    .all = FALSE,
                    .test = TRUE,
                    .adjust = "none",
                    .type = "parametric",
                    .bullet = " * ")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "a", "b", "p-value"))
    expect_equal(res$name,
                 c("p", "q"))
    expect_equal(res$a,
                 c("0.94 ± 3.08", "-0.17 ± 1.00"))
    expect_equal(res$b,
                 c("10.23 ± 4.41", "0.13 ± 0.87"))
    expect_equal(res$`p-value`,
                 c("<0.0001", "NS"))
})


test_that("baseline without grouping", {
    set.seed(333)
    n <- 30
    d <- data.frame(p = c(rnorm(n, 1, 3), rnorm(n, 10, 5)),
                    q = rnorm(2 * n),
                    g = factor(c(rep("a", n), rep("b", n))))
    res <- baseline(d,
                    .n = TRUE,
                    .all = TRUE,
                    .test = FALSE,
                    .type = "parametric",
                    .bullet = " * ")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "all"))
    expect_equal(res$name,
                 c("N", "p", "q", "g:", " * a", " * b"))
    expect_equal(res$all,
                 c("60", "5.59 ± 6.01", "-0.02 ± 0.94", "", "30 (50.00 %)", "30 (50.00 %)"))
})


test_that("baseline max_width", {
    d <- data.frame(abcdefghijklmn = factor(c("1234567890", "0987654321")))
    res <- baseline(d,
                    .n = FALSE,
                    .all = TRUE,
                    .test = FALSE,
                    .type = "robust",
                    .max_width = 5,
                    .bullet = " * ")

    expect_true(is.data.frame(res))
    expect_equal(colnames(res),
                 c("name", "all"))
    expect_equal(res$name,
                 c("abcde...:", " * 09876...", " * 12345..."))
})
