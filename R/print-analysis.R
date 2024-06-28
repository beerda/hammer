#'
#' @return
#' @author Michal Burda
#' @export
#' @me
print.report <- function(x, ...) {
    lapply(x, function(item) {
        print(item)
        cat0("\n\n")
    })

    invisible(x)
}
