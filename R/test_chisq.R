#' Return p-value of the Chi-squared test
#'
#' This function is a wrapper over the [chisq.test()] function.
#' It performs Chi-squared goodness-of-fit test and returns the resulting p-value.
#'
#' @param x data factor
#' @param g grouping factor
#' @param ... further parameters passed to the underlying [chisq.test()] function
#' @return The p-value of the test
#' @author Michal Burda
#' @export
test_chisq <- function(x, g, ...) {
    .must_be_factor(x)
    .must_be_factor(g)
    .must_have_equal_lengths(x, g)

    fit <- chisq.test(x, g, ...)

    fit$p.value
}
