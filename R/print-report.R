#'
#' @return
#' @author Michal Burda
#' @export
#' @me
print.analysis <- function(x, ...) {
    lapply(x, function(item) {
        print(item)
        cat0("\n\n")
    })

    invisible(x)
}
