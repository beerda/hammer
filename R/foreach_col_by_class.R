#'
#' @return
#' @author Michal Burda
#' @export
foreach_col_by_class <- function(.data, .handlers = list(), .fail = NULL, ...) {
    .must_be_data_frame(.data)
    .must_be_list_of_functions(.handlers)

    if (is.null(names(.handlers))) {
        cli_abort(c("{.var .handlers} must be a named list.",
                    "x" = "{.var names(.handlers)} is {.value NULL}."))
    }

    dots <- list(...)

    f <- function(x) {
        res <- .fail
        cl <- class(x)
        i <- which(cl %in% names(.handlers))
        if (length(i) > 0) {
            handler <- .handlers[[cl[i[1]]]]
            res <- do.call(handler, c(list(x), dots))
        }

        res
    }

    lapply(.data, f)
}
