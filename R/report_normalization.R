#'
#' @return
#' @author Michal Burda
#' @export
report_normalization <- function(data) {
    isVector <- FALSE
    if (is.vector(data)) {
        .must_be_numeric_vector(data)
        data <- data.frame(data)
        isVector <- TRUE
    }

    .must_be_data_frame(data)

    res <- data.frame(variable = colnames(data))
    res$transform <- sapply(data, function(x) {
        a <- attr(x, "normalized:method")
        if (is.null(a)) {
            return(NA)
        } else {
            return(class(a$chosen_transform)[1])
        }
    })

    res
}
