#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom tidyselect eval_select
#' @importFrom tidyselect everything
#' @importFrom tidyselect enquo
#' @importFrom bestNormalize bestNormalize
normalize <- function(data,
                      what = tidyselect::everything(),
                      normalize = TRUE,
                      center = TRUE,
                      scale = TRUE,
                      ...) {
    isVector <- FALSE
    if (is.vector(data)) {
        .must_be_numeric_vector(data)
        data <- data.frame(result = data)
        isVector <- TRUE
        loc <- 1
    } else {
        loc <- tidyselect::eval_select(
            enquo(what),
            data = data
        )
    }

    .must_be_data_frame(data)
    .must_be_flag(normalize)
    .must_be_flag(center)
    .must_be_flag(scale)

    for (i in loc) {
        val <- data[[i]]
        norm <- NULL
        if (is.numeric(val)) {
            if (normalize) {
                loo <- length(na.omit(val)) < 30
                norm <- bestNormalize(val, loo = loo, ...)
                val <- norm$chosen_transform$x.t
            }
            if (center || scale) {
                val <- scale(val, center = center, scale = scale)
                dim(val) <- dim(val)[1] # make a vector from matrix
            }
            if (normalize) {
                attr(val, 'normalized:method') <- norm
            }
        }

        data[[i]] <- val
    }

    if (isVector) {
        data <- data$result
    }

   data
}
