#' Convert the numeric vector to character vector
#'
#' @param x the numeric vector to be converted
#' @param digits the number of decimal point numbers to round the numbers to
#' @param na the string to be used for `NA` values
#' @param ... further arguments that are ignored
#' @returns a character vector of transformed numeric values
#'
#' @author Michal Burda
#' @export
format_number <- function(x,
                          digits = 2,
                          na = "NA",
                          style = "signif",
                          ...) {
  .must_be_numeric_vector(x)
  .must_be_integerish_scalar(digits)
  .must_be_greater_eq(digits, 0)
  .must_be_character_scalar(na)
  .must_be_character_scalar(style)
  .must_be_enum(style, c("signif", "fixed"))

  result <- NULL
  if (digits == 0) {
      result <- format(round(x, digits = 0),
                       nsmall = 0,
                       scientific = FALSE,
                       trim = TRUE)
  } else {
      r <- format(round(x, digits = digits),
                  nsmall = digits,
                  scientific = FALSE,
                  trim = TRUE)

      if (style == "fixed") {
          result <- r
      } else {
          f <- function(x) {
              format(signif(x, digits = digits),
                     nsmall = digits,
                     scientific = FALSE,
                     trim = TRUE)
          }
          s <- vapply(x, f, character(1))

          result <- ifelse(abs(x) >= 0.1, r, s)
      }
  }

  result[x == 0] <- "0"
  result[is.na(x)] <- na

  result
}
