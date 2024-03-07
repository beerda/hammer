#' Compute the mean of the given numeric vector
#'
#' The function returns the formatted `"mean"`
#' of vector `x`.
#'
#' @param x the vector to be processed
#' @param ... further arguments passed to [format_number()]
#' @return The formatted string containing the mean.
#' @seealso [aggreg_mean_sd()]
#' @author Michal Burda
#' @export
aggreg_mean <- function(x, ...) {
    .must_be_numeric_vector(x)

    m <- mean(x, na.rm = TRUE)

    format_number(m, ...)
}
