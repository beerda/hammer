#' Compute the mean and standard deviation of the given numeric vector
#'
#' The function returns the formatted `"mean ± stdev"`
#' of column `var` of data frame `data`.
#'
#' @param data the data frame
#' @param var the name of the column to be processed
#' @param ... further arguments passed to [format_number()]
#' @return The formatted string containing the mean and standard deviation.
#' @seealso [aggreg_median_iqr()]
#' @author Michal Burda
#' @export
aggreg_mean_sd <- function(data, var, ...) {
    .must_be_data_frame(data)
    .must_be_string(var)

    x <- data[[var]]

    .must_be_numeric_vector(x)

    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)

    paste(format_number(m, ...),
          '\u00B1',               # plusminus sign
          format_number(s, ...))
}
