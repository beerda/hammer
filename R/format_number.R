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