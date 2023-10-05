#'
#' @return
#' @author Michal Burda
#' @export
impute <- function(.data,
                   .selection = everything(),
                   .f,
                   ...) {
    .must_be_data_frame(.data)
    .must_be_function(.f)

    .selection = enquo(.selection)

    f <- function(x) {
        i <- is.na(x)
        if (any(i)) {
            val <- .f(na.omit(x), ...)
            x <- replace(x, is.na(x), val)
        }

        x
    }

    mutate(.data, across(!!.selection, f))
}
