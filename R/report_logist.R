#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom logistf logistf
report_logist <- function(data, formula, firth = FALSE, digits = 3) {
    .must_be_data_frame(data)
    .must_be_formula_with_lhs(formula)
    .must_be_flag(firth)
    .must_be_integerish_scalar(digits)

    if (firth) {
        fit <- logistf(formula, data = data)
        variable <- names(fit$coefficients)
        estimate <- fit$coefficients
        error <- sqrt(diag(fit$var))
        p <- fit$prob
    } else {
        fit <- glm(formula, data = data, family = binomial)
        s <- summary(fit)
        variable <- rownames(coef(s))
        estimate <- coef(s)[, 'Estimate']
        error <- coef(s)[, 'Std. Error']
        p <- coef(s)[, 'Pr(>|z|)']
    }

    ci <- confint(fit)
    odds_ratio <- exp(estimate)
    odds_ci <- exp(ci)

    res <- tibble(variable = variable,
                  estimate = format_number(estimate, digits),
                  error = format_number(error, digits),
                  `odds ratio` = format_number(odds_ratio, digits),
                  `odds ratio 95% CI` = format_interval(odds_ci[, 1], odds_ci[, 2], digits = digits),
                  p = format_pvalue(p))
    attr(res, 'fit') <- fit

    res
}
