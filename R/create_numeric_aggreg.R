#' Factory function for creation of aggregation function of numeric vectors
#'
#' Function creates a function for aggregating numeric vectors into a single
#' character value. It creates three types of aggregators: robust, parametric,
#' or test-based accordingly to the `type` argument.
#'
#' @param type selection of the statistic to compute for numeric
#'      variables: `"robust"` (see [aggreg_median_iqr()]), `"parametric"` (see
#'      [aggreg_mean_sd()]), or `"test"`, which performs `.normality_test` first
#'      on each numeric variable to determine between `"robust"` and `"parametric"`
#' @param parametric_aggreg which aggregator to use as a parametric one
#' @param robust_aggreg which aggregator to use as a robust one
#' @param normality_test a function that performs the normality test in case of
#'      `type = "test"`. The function must return a list with the `"p.value"`
#'      element containing the p-value of the rejection of normality.
#' @param normality_thresh a p-value threshold below which will be the
#'      `normality_test` interpreted as rejection of normality.
#' @return An aggregator function
#' @seealso [aggreg_mean_sd()], [aggreg_median_iqr()]
#' @author Michal Burda
#' @export
#' @examples
#' aggreg <- create_numeric_aggreg("robust")
#' aggreg(1:10)
#'
create_numeric_aggreg <- function(type = c("robust", "parametric", "test"),
                                  parametric_aggreg = aggreg_mean_sd,
                                  robust_aggreg = aggreg_median_iqr,
                                  normality_test = shapiro.test,
                                  normality_thresh = 0.05) {
    type <- match.arg(type)

    .must_be_character_scalar(type)
    .must_be_function(normality_test)
    .must_be_function(parametric_aggreg)
    .must_be_function(robust_aggreg)
    .must_be_double_scalar(normality_thresh)
    .must_be_in_range(normality_thresh, c(0, 0.1))

    if (type == "robust")
        return(robust_aggreg)

    if (type == "parametric")
        return(parametric_aggreg)

    function(x, ...) {
        fit <- normality_test(x)
        if (fit$p.value < normality_thresh)
            return(robust_aggreg(x, ...))
        else
            return(parametric_aggreg(x, ...))
    }
}

