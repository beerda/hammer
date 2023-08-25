#' Factory function for creation of test function for numeric vectors
#'
#' Function creates a function for statistical testing of numeric vectors
#' by groups. It creates three types of test functions: robust, parametric,
#' or test-based accordingly to the `type` argument.
#'
#' @param type selection of the type of statistical test for numeric
#'      variables: `"robust"` (see [test_wilcox()] or [test_kruskal()]),
#'      `"parametric"` (see [test_t()] or [test_aov()]), or `"test"`, which
#'      performs `.normality_test` first on the numeric variable to select from
#'      `"robust"` and `"parametric"`
#' @param k the number of groups for which the test will be performed. The value
#'      must be greater than 1. If `k` equals `2`, two-sample tests are returned
#'      (either [test_t()] or [test_wilcox()]). For `k` greater than `2`, either
#'      [test_aov()] or [test_kruskal()] is returned.
#' @param parametric_test which test to use as a parametric one
#' @param robust_test which test to use as a robust one
#' @param normality_test a function that performs the normality test in case of
#'      `type = "test"`. The function must return a list with the `"p.value"`
#'      element containing the p-value of the rejection of normality.
#' @param normality_thresh a p-value threshold below which will be the
#'      `normality_test` interpreted as rejection of normality.
#' @return A test function
#' @seealso [test_t()], [test_wilcox()], [test_kruskal()], [test_aov()]
#'
#' @return a test function
#' @author Michal Burda
#' @export
create_numeric_test <- function(type = c("robust", "parametric", "test"),
                                k = 2,
                                parametric_test = if (k > 2) test_aov else test_t,
                                robust_test = if (k > 2) test_kruskal else test_wilcox,
                                normality_test = shapiro.test,
                                normality_thresh = 0.05) {
    type <- match.arg(type)

    .must_be_character_scalar(type)
    .must_be_integerish_scalar(k)
    .must_be_greater(k, 1)
    .must_be_function(normality_test)
    .must_be_function(parametric_test)
    .must_be_function(robust_test)
    .must_be_double_scalar(normality_thresh)
    .must_be_in_range(normality_thresh, c(0, 0.1))

    if (type == "robust")
        return(robust_test)

    if (type == "parametric")
        return(parametric_test)

    function(x, y, ...) {
        fit <- normality_test(c(x, y))
        if (fit$p.value < normality_thresh)
            return(robust_test(x, y, ...))
        else
            return(parametric_test(x, y, ...))
    }
}
