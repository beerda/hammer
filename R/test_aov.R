#' Return p-value of the ANOVA test
#'
#' This function is a wrapper over the [aov()] function.
#' It performs the analysis of variance and returns the resulting p-value.
#'
#' @param x numeric values
#' @param g grouping factor
#' @param confounders `NULL` or a list of confounders, i.e. a list of numeric
#'      vectors of the same lengths as `x` and `g`
#' @param ... further parameters passed to the underlying [aov()] function
#' @return The p-value of the test
#' @author Michal Burda
#' @export
test_aov <- function(x, g, confounders = NULL, ...) {
    .must_be_numeric_vector(x)
    .must_be_factor(g)
    .must_be_list(confounders, null = TRUE)
    .must_have_equal_lengths(x, g)

    formula <- "x ~ g"

    if (length(confounders) > 0) {
        .must_be_list_of_doubles(confounders)
        .must_be_list_of_equal_length_vectors(confounders)
        .must_have_equal_lengths(x, confounders[[1]])

        names(confounders) <- paste0("v", seq_along(confounders))
        formula <- paste(formula,
                         "+",
                         paste(names(confounders), collapse = "+"))
    } else {
        confounders <- list()
    }

    confounders$x <- x
    confounders$g <- g
    d <- as.data.frame(confounders)
    fit <- aov(as.formula(formula), data = d, ...)
    s <- summary(fit)

    s[[1]][, 5][1]
}
