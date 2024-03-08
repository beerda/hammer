#' Call function with arguments as a list while possibly ignoring unused arguments.
#'
#' @param fun the function to be called
#' @param ... further arguments to be passed to `fun`
#' @param .args the named list of arguments
#' @param .ignore_unused if TRUE, ignore arguments in `args` that are not accepted by `fun`.
#'      If `fun` accepts `...` arguments, nothing is ignored.
#' @param .quote a logical value indicating whether to quote the arguments.
#' @param .envir an environment within which to evaluate the call. This will be most useful
#'      if what is a character string and the arguments are symbols or quoted expressions.
#' @return The result of the function `fun`
#' @seealso [base::do.call()]
#' @author Michal Burda
#' @export
do_call <- function(fun,
                    ...,
                    .args = list(),
                    .ignore_unused = TRUE,
                    .quote = FALSE,
                    .envir = parent.frame()) {
    .must_be_function(fun)
    .must_be_list(.args)
    .must_be_logical_scalar(.ignore_unused)
    .must_be_logical_scalar(.quote)

    dots <- list(...)
    args <- c(.args, dots)

    if (.ignore_unused) {
        funArgs <- names(formals(fun))
        if (!("..." %in% funArgs)) {
            keep <- intersect(names(args), funArgs)
            args <- args[keep]
        }
    }

    do.call(what = fun,
            args = args,
            quote = .quote,
            envir = .envir)
}
