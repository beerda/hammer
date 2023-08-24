#' Return p-value of the two-sample t test
#'
#' This function is a wrapper over the [t.test()] function.
#' It performs the two-sample t test and returns the resulting p-value.
#'
#' @param x values of the first sample
#' @param y values of the second sample
#' @param ... further parameters passed to the underlying [t.test()] function
#' @return The p-value of the test
#' @author Michal Burda
#' @export
test_t <- function(x, y, ...) {
    .must_be_numeric_vector(x)
    .must_be_numeric_vector(y)

    fit <- t.test(x, y, ...)

    fit$p.value
}
