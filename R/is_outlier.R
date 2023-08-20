#' Test whether the values are outliers
#'
#' @param x the numeric vector to be tested
#' @param extreme `TRUE` if test for extreme outliers
#' @returns a logical vector
#' @export
#' @author Michal Burda
is_outlier <- function(x,
                       extreme = FALSE) {
    .must_be_numeric_vector(x)
    .must_be_flag(extreme)

    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    out <- q + c(-1, 1) * iqr * ifelse(extreme, 3, 1.5)

    x < out[1] | x > out[2]
}
