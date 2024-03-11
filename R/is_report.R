#'
#' @return
#' @author Michal Burda
#' @export
is_report <- function(x) {
    inherits(x, "report") && is.list(x)
}
