#' Return p-value of Kruskal-Wallis rank sum test
#'
#' This function is a wrapper over the [kruskal.test()] function.
#' It performs the Kruskal-Wallis rank sum test and returns the resulting p-value.
#'
#' @param x numeric values
#' @param g grouping factor
#' @param ... further parameters passed to the underlying [kruskal.test()] function
#' @return The p-value of the test
#' @author Michal Burda
#' @export
test_kruskal <- function(x, g, ...) {
    .must_be_numeric_vector(x)
    .must_be_factor(g)
    .must_have_equal_lengths(x, g)

    fit <- kruskal.test(x, g, ...)

    fit$p.value
}
