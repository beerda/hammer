#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom stats kruskal.test
#' @importFrom rstatix kruskal_effsize
#' @importFrom rstatix dunn_test
#' @importFrom rstatix anova_test
#' @importFrom rstatix pairwise_t_test
report_oneway <- function(data,
                          var,
                          group,
                          id = NULL,
                          omit_levels = NULL,
                          type = c("robust", "parametric"),
                          paired = FALSE,
                          adjust = p.adjust.methods,
                          boxplot = TRUE,
                          digits = 2,
                          thresh = 0.1) {
    type = match.arg(type)

    .must_be_data_frame(data)
    .must_be_character_scalar(var)
    .must_be_character_scalar(group)
    .must_be_character_vector(omit_levels, null = TRUE)
    .must_be_flag(paired)
    .must_be_flag(boxplot)

    .must_be_factor(data[[group]], name = "data[[group]]")

    data <- data[!is.na(data[[group]]) & !is.na(data[[var]]), ]
    if (!is.null(omit_levels)) {
        data <- data[!(data[[group]] %in% omit_levels), ]
    }

    res <- list()
    f <- as.formula(paste(var, '~', group))

    if (boxplot == TRUE) {
        res[["boxplot"]] <- ggplot(data) +
            aes(x = .data[[group]], y = .data[[var]]) +
            geom_boxplot() +
            geom_jitter(width = 0.1, color = "gray", alpha = 0.3) +
            coord_flip() +
            xlab("") + ylab("") +
            theme(legend.position = "none")
    }

    if (type == "robust") {
        if (paired) {
            stop("not implemented robust + paired")

        } else {
            fit <- kruskal.test(formula = f, data = data)
            eff <- kruskal_effsize(data = data, formula = f)
            tab <- dunn_test(data = data, formula = f, detailed = TRUE)
            tab$effect <- abs(tab$statistic) / sqrt(tab$n1 + tab$n2)
            tab$effect = ifelse(tab$effect < 0.3, "small",
                                ifelse(tab$effect >= 0.5, "large", "moderate"))
            tab$statistic <- format_number(tab$statistic, digits = digits)
            tab$`mean rank diff` = format_number(tab$estimate, digits = digits)
            tab$p <- format_pvalue(tab$p, thresh = thresh)
            tab$p.adj <- format_pvalue(tab$p.adj, thresh = thresh)
            tab <- tab[, c("group1", "group2", "n1", "n2", "mean rank diff", "p", "p.adj", "effect")]

            res[["test"]] <- paragraph(
                "Kruskal-Wallis rank sum test: **",
                format_pvalue(fit$p.value, varname = "p", thresh = thresh),
                "** ($\\chi^2$ = ",
                format_number(fit$statistic, digits = digits),
                ", df = ",
                fit$parameter,
                ")")
            res[["effect"]] <- paragraph(
                "Kruskal-Wallis Effect Size: **",
                paste0(eff$magnitude),
                "** ($\\eta^2$ = ",
                format_number(eff$effsize, digits = digits),
                ", ",
                format_number(eff$effsize * 100, digits = digits),
                " % of explained variance)")
            res[["posthoc_title"]] <- paragraph("Dunn's test:")
            res[["posthoc"]] <- kable(tab)
        }

    } else if (type == "parametric") {
        if (paired) {
            fit <- anova_test(data = data, dv = !!sym(var), wid = !!sym(id), within = !!sym(group))
            if (!is.null(fit$ANOVA)) {
                fit <- fit$ANOVA
            }

            eff <- cut(fit$ges,
                       breaks = c(-Inf, 0.01, 0.06, 0.14, Inf),
                       labels = c("negligible", "small", "moderate", "large"),
                       right = FALSE)
            tab <- pairwise_t_test(data = data,
                                   formula = f,
                                   paired = TRUE,
                                   p.adjust.method = adjust)
            tab$p <- format_pvalue(tab$p, thresh = thresh)
            tab$p.adj <- format_pvalue(tab$p.adj, thresh = thresh)
            tab <- tab[, c("group1", "group2", "n1", "n2", "p", "p.adj")]

            res[["test"]] <- paragraph(
                "One-way repeated measures ANOVA: **",
                format_pvalue(fit$p, varname = "p"),
                "** (F = ",
                format_number(fit$F),
                ", df = (",
                fit$DFn, ", ", fit$DFd,
                "))\n\n")
            res[["effect"]] <- paragraph(
                "Effect size: **",
                as.character(eff),
                "** ($\\eta^2$ = ",
                format_number(fit$ges),
                ", ",
                format_number(fit$ges * 100),
                " % of explained variance)")
            res[["posthoc_title"]] <- paragraph("Pairwise paired t test:")
            res[["posthoc"]] <- kable(tab)

        } else {
            stop("not implemented parametric + !paired")
        }

    } else {
        stop("not implemented type = ", type)
    }

    report(res)
}
