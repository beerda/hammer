#' Return p-value of the two-sample Wilcoxon rank sum test
#'
#' This function is a wrapper over the [wilcox.test()] function.
#' It performs the two-sample Wilcoxon rank sum test and returns
#' the resulting p-value.
#'
#' @param x values of the first sample
#' @param y values of the second sample
#' @param ... further parameters passed to the underlying [wilcox.test()] function
#' @return The p-value of the test
#' @author Michal Burda
#' @export
test_wilcox <- function(x, y, ...) {
    .must_be_numeric_vector(x)
    .must_be_numeric_vector(y)

    fit <- wilcox.test(x, y, ...)

    fit$p.value
}
