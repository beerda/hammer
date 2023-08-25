#' Return p-value of the two-sample Wilcoxon rank sum test
#'
#' This function is a wrapper over the [wilcox.test()] function.
#' It performs the two-sample Wilcoxon rank sum test and returns
#' the resulting p-value.
#'
#' @param x numeric values
#' @param g grouping factor (must contain 2 levels)
#' @param ... further parameters passed to the underlying [wilcox.test()] function
#' @return The p-value of the test
#' @author Michal Burda
#' @export
test_wilcox <- function(x, g, ...) {
    .must_be_numeric_vector(x)
    .must_be_factor(g)
    .must_have_length(levels(g), 2)
    .must_have_equal_lengths(x, g)

    a <- x[as.integer(g) == 1]
    b <- x[as.integer(g) == 2]
    fit <- wilcox.test(a, b, ...)

    fit$p.value
}
