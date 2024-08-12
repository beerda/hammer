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
    .must_be_numeric_vector_or_ordered_factor(x)
    .must_be_factor(g)
    .must_have_length(levels(g), 2)
    .must_have_equal_lengths(x, g)

    x <- as.numeric(x)
    g <- as.integer(g)
    a <- x[g == 1]
    b <- x[g == 2]
    fit <- wilcox.test(a, b, ...)

    fit$p.value
}
