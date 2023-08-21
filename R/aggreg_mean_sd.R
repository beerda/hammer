#' Compute the mean and standard deviation of the given numeric vector
#'
#' The function returns the formatted `"mean ± stdev"`
#' of vector `x`.
#'
#' @param x the vector to be processed
#' @param ... further arguments passed to [format_number()]
#' @return The formatted string containing the mean and standard deviation.
#' @seealso [aggreg_median_iqr()]
#' @author Michal Burda
#' @export
aggreg_mean_sd <- function(x, ...) {
    .must_be_numeric_vector(x)

    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)

    paste(format_number(m, ...),
          '\u00B1',               # plusminus sign
          format_number(s, ...))
}
