#' Generate a table describing all columns in a data frame
#'
#' @param .data a data frame
#' @param ... other agruments passed to [format_count_percent()],
#'      [format_interval()] or [format_string()]
#' @returns a description of columns as tibble
#' @author Michal Burda
#' @export
report_columns <- function(.data,
                           ...) {
    .must_be_data_frame(.data)

    res <- tibble()

    for (var in colnames(.data)) {
        x <- .data[[var]]

        r <- tibble(variable = format_string(var, ...),
                    class = paste(class(x), collapse = " "),
                    values = format_count_percent(sum(!is.na(x)), length(x), ...),
                    `N/A` = format_count_percent(sum(is.na(x)), length(x), ...),
                    details = "")

        if (is.numeric(x)) {
            r$details <- paste("range:",
                               format_interval(min(x, na.rm = TRUE),
                                               max(x, na.rm = TRUE),
                                               ...))
        } else if (is.factor(x)) {
            r$details <- paste("levels:", nlevels(x))
        } else {
            r$details <- paste("unique:", length(unique(x)))
        }

        new_row(res) <- r

        if (is.factor(x) || is.logical(x)) {
            if (is.logical(x)) {
                x <- factor(x, levels = c("FALSE", "TRUE"))
            }
            for (l in levels(x)) {
                r <- tibble(variable = paste0("\u2022 ", format_string(l, ...)),
                            class = "",
                            values = aggreg_count_percent(data.frame(x = x), "x", level = l, ...),
                            `N/A` = "",
                            details = "")
                new_row(res) <- r
            }
        }
    }

    res
}
