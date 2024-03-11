#'
#' @return
#' @author Michal Burda
#' @export
print.paragraph <- function(x, ...) {
    cat0(unlist(x), sep = attr(x, "sep"))
}
