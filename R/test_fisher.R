#' Return p-value of Fisher's test
#'
#' This function is a wrapper over the [fisher.test()] function.
#' It performs Fisher's test and returns the resulting p-value.
#'
#' @param x data factor
#' @param g grouping factor
#' @param ... further parameters passed to the underlying [fisher.test()] function
#' @return The p-value of the test
#' @author Michal Burda
#' @export
test_fisher <- function(x, g, ...) {
    .must_be_factor_or_logical(x)
    .must_be_factor(g)
    .must_have_equal_lengths(x, g)

    fit <- do_call(fisher.test, x = x, y = g, ...)

    fit$p.value
}
