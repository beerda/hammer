#'
#' @return
#' @author Michal Burda
#' @export
create_numeric_test <- function(type = c("robust", "parametric", "test"),
                                parametric_test = test_t,
                                robust_test = test_wilcox,
                                normality_test = shapiro.test,
                                normality_thresh = 0.05) {
    type <- match.arg(type)

    .must_be_character_scalar(type)
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
