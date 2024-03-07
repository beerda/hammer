#' Compute the median of the given numeric vector
#'
#' The function returns the formatted "median"
#' of vector `x`.
#'
#' @param x the vector to be processed
#' @param ... further arguments passed to [format_number()]
#' @return The formatted string containing the median.
#' @seealso [aggreg_median_iqr()]
#' @author Michal Burda
#' @export
aggreg_median <- function(x, ...) {
    .must_be_numeric_vector(x)

    m <- median(x, na.rm = TRUE)

    format_number(m, ...)
}
