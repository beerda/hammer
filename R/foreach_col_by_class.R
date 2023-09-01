#' Apply a function selected by class for each column of data frame
#'
#' For each column of `.data`, an appropriate callback function is selected
#' from `.handlers` and executed. Selection is based on column class and
#' names of the elements of `.handlers`: first element (function) in `.handlers`,
#' that has a name as some class of the processed column, will be executed.
#' If search for the callback function fails, the value of `.fail` will be
#' used as a result.
#'
#' @param .data the data frame to be processed
#' @param .handlers the named list of callback functions (the names have to
#'      correspond to `.data` column classes)
#' @param .fail the value returned if the callback function is not found
#' @param ... further arguments passed to the callback function
#' @return The list of results of function calls or `.fail` if the proper
#'      callback function cannot be found in `.hanlders`.
#' @author Michal Burda
#' @export
#' @examples
#' df <- data.frame(a = factor(c("a", "a", "b", "b")),
#'                  b = 1:4,
#'                  c = 11:14)
#' foreach_col_by_class(df, list(factor = nlevels,
#'                               integer = sum))
#' # returns list(a = 2, c = 50)
foreach_col_by_class <- function(.data,
                                 .handlers = list(),
                                 .fail = NULL,
                                 ...) {
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
