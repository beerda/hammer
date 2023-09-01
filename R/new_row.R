#' Append new rows to a data frame
#'
#' @param data a data frame to append the rows to
#' @param value new data to be apended to `data`
#' @export
`new_row<-` <- function(data, value) {
    .must_be_data_frame(data)
    .must_be_data_frame(value)

    bind_rows(data, value)
}
