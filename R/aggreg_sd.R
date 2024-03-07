#' Compute the standard deviation of the given numeric vector
#'
#' The function returns the formatted `"stdev"`
#' of vector `x`.
#'
#' @param x the vector to be processed
#' @param ... further arguments passed to [format_number()]
#' @return The formatted string containing the standard deviation.
#' @seealso [aggreg_median_iqr()]
#' @author Michal Burda
#' @export
aggreg_sd <- function(x, ...) {
    .must_be_numeric_vector(x)

    s <- sd(x, na.rm = TRUE)

    format_number(s, ...)
}
