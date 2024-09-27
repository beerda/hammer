#' Compute the median and lower/upper quartile of the given numeric vector
#'
#' The function returns the formatted "median (lower_quartile -- upper_quartile)"
#' of column `var` in data frame `data`.
#'
#' @param data the data frame
#' @param var the name of the column to be processed
#' @param ... further arguments passed to [format_number()]
#' @return The formatted string containing the median and lower/upper quartile.
#' @seealso [aggreg_mean_sd()]
#' @author Michal Burda
#' @export
aggreg_median_iqr <- function(data, var, ...) {
    .must_be_data_frame(data)
    .must_be_string(var)

    x <- data[[var]]

    .must_be_numeric_vector(x)

    m <- median(x, na.rm = TRUE)
    q1 <- quantile(x, probs = 0.25, na.rm = TRUE)
    q3 <- quantile(x, probs = 0.75, na.rm = TRUE)

    paste0(format_number(m, ...),
           ' (',
           format_interval(q1, q3, ...),
           ')')
}
