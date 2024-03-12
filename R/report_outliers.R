#' Generate a summary table of outliers in numeric columns of a data frame
#'
#' @param .data the data frame
#' @return a summary of outliers as a tibble
#' @author Michal Burda
#' @export
report_outliers <- function(.data, id, digits = 2) {
    .must_be_data_frame(.data)
    .must_be_character_scalar(id)  # TODO: allow tidyeval

    res <- tibble()
    idData <- .data[[id]]

    groups <- list(all = seq_len(nrow(.data)))
    if (n_groups(.data) > 1) {
        groups <- group_rows(.data)
        names(groups) <- Reduce(function(...) paste(..., sep='/'),
                                group_keys(.data))
    }

    for (var in colnames(.data)) {
        valueData <- .data[[var]]

        for (gi in seq_along(groups)) {
            gname <- names(groups)[gi]
            gids <- idData[groups[[gi]]]
            x <- valueData[groups[[gi]]]

            if (is.numeric(x)) {
                out <- is_outlier(x)
                if (any(out)) {
                    out <- sort(unique(x[out]))
                    ex <- unique(x[is_outlier(x, TRUE)])
                    for (o in out) {
                        ids <- gids[x == o]
                        r <- tibble(variable = var,
                                    group = gname,
                                    value = format_number(o, digits = digits),
                                    type = ifelse(o %in% ex, "extreme", "outlier"),
                                    `median (Q1 - Q3)` = aggreg_median_iqr(x, digits = digits),
                                    ids = format_enumeration(ids),
                                    count = format_count_percent(sum(x == o), length(x)))
                        new_row(res) <- r
                        var <- ""
                        #gname <- ""
                    }
                }
            }
        }
    }

    colnames(res)[colnames(res) == "ids"] <- id

    if (n_groups(.data) <= 1) {
        res$group <- NULL
    }

    res
}
