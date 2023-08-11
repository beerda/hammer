#' Convert the numeric vector of p-values to formatted character vector
#'
#' P-values in `x` are formatted as strings with given number of `digits`.
#' If the p-value is greater than the given threshold `thresh`, the `"NS"`
#' string is returned instead (or whathever is set in the `ns` argument).
#' If the p-value is lower than the lowest number that fits to the given
#' number of `digits`, a string similar to`"<0.0001"` (having the appropriate
#' number of digits) is returned. If the p-value is `NA`, the `"NA"` string
#' is returned (or whatever is set in the `na` argument).
#'
#' If `varname` argument is not an empty string, the p-values are formatted
#' as `"<varname>=<value>"`.
#'
#' @param x the numeric vector to be converted
#' @param digits the number of decimal point numbers to round the numbers to
#' @param na the string to be used for `NA` values
#' @returns a character vector of transformed numeric values
#'
#' @export
#' @author Michal Burda
format_pvalue <- function(x,
                          digits = 4,
                          thresh = 0.05,
                          varname = '',
                          ns = 'NS',
                          na = 'NA') {
    .must_be_numeric_vector(x)
    .must_be_integerish_scalar(digits)
    .must_be_greater_eq(digits, 0)
    .must_be_double_scalar(thresh)
    .must_be_in_range(thresh, c(0, 1))
    .must_be_character_scalar(varname)
    .must_be_character_scalar(ns)
    .must_be_character_scalar(na)

    eq <- ifelse(varname != '', '=', '')
    na <- paste0(varname, eq, na)
    ns <- paste0(varname, eq, ns)
    res <- vapply(x, function(p) {
        if (is.na(p)) {
            return(na)
        }
        if (p > thresh) {
            return(ns)
        }
        if (p < 1/10^digits) {
            return(paste0(varname, '<', format(1/10^digits, nsmall=digits, scientific=FALSE)))
        } else {
            return(paste0(varname, eq, format_number(p, digits)))
        }
    }, character(1))

    res
}
