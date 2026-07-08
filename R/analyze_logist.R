#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom logistf logistf
analyze_logist <- function(data,
                           formula,
                           firth = FALSE,
                           digits = 2,
                           thresh = 1.00,
                           figData = NULL,
                           aes = NULL) {
    .must_be_data_frame(data)
    .must_be_formula_with_lhs(formula)
    .must_be_flag(firth)
    .must_be_integerish_scalar(digits)
    .must_be_data_frame(figData, null = TRUE)

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

    ci <- suppressMessages(confint(fit))
    odds_ratio <- exp(estimate)
    odds_ci <- exp(ci)

    res <- list()
    res$formula <- paragraph("Formula: ", as.character(formula))
    mdl <- tibble(variable = variable,
                  estimate = format_number(estimate, digits),
                  error = format_number(error, digits),
                  `odds ratio` = format_number(odds_ratio, digits),
                  `odds ratio 95% CI` = format_interval(odds_ci[, 1], odds_ci[, 2], digits = digits),
                  p = format_pvalue(p, thresh = thresh))
    res$model <- kable(mdl, align = "r")

    if (!is.null(aes)) {
        data[["#predicted#"]] <- predict(fit, data, type = "response")
        res$predicted_plot1 <- ggplot(data) +
            aes(y = `#predicted#`) +
            aes +
            geom_point(na.rm = TRUE) +
            ylab("predicted probability")
        if (!is.null(figData)) {
            figData[["#predicted#"]] <- predict(fit, figData, type = "response")
            res$predicted_plot2 <- ggplot(figData) +
                aes(y = `#predicted#`) +
                aes +
                geom_point(na.rm = TRUE) +
                ylab("predicted probability")
        }
    }

    attr(res, "fit") <- fit

    analysis(res)
}
