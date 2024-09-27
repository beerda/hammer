#' Compute the mean of the given numeric vector
#'
#' The function returns the formatted `"mean"`
#' of column `var` in data frame `data`.
#'
#' @param data the data frame
#' @param var the name of the column to be processed
#' @param ... further arguments passed to [format_number()]
#' @return The formatted string containing the mean.
#' @seealso [aggreg_mean_sd()]
#' @author Michal Burda
#' @export
aggreg_mean <- function(data, var, ...) {
    .must_be_data_frame(data)
    .must_be_string(var)

    x <- data[[var]]

    .must_be_numeric_vector(x)

    m <- mean(x, na.rm = TRUE)

    format_number(m, ...)
}
