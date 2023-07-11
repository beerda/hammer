#' Convert the numeric vector to character vector
#'
#' @param x the numeric vector to be converted
#' @param digits the number of decimal point numbers to round the numbers to
#' @param na the string to be used for `NA` values
#' @returns a character vector of transformed numeric values
#'
#' @author Michal Burda
#' @export
format_number <- function(x,
                          digits = 2,
                          na = "NA") {
  .must_be_numeric_vector(x)
  .must_be_integerish_scalar(digits)
  .must_be_greater_eq(digits, 0)
  .must_be_character_scalar(na)

  result <- format(round(x, digits = digits),
                   nsmall = digits,
                   scientific = FALSE,
                   trim = TRUE)
  result[is.na(x)] <- na

  result
}
