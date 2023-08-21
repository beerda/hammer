#' Generate a summary table of outliers in numeric columns of a data frame
#'
#' @param .data the data frame
#' @return a summary of outliers as a tibble
#' @author Michal Burda
#' @export
report_outliers <- function(.data) {
    .must_be_data_frame(.data)

    res <- tibble()

    for (var in colnames(.data)) {
        x <- .data[[var]]

        if (is.numeric(x)) {
            out <- is_outlier(x)
            if (any(out)) {
                out <- sort(unique(x[out]))
                ex <- unique(x[is_outlier(x, TRUE)])
                for (o in out) {
                    r <- tibble(variable = var,
                                outlier = as.character(o),
                                type = ifelse(o %in% ex, "extreme", "outlier"),
                                count = format_count_percent(sum(x == o), length(x)))
                    new_row(res) <- r
                    var <- ""
                }
            }
        }
    }

    res
}
