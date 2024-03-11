#'
#' @return
#' @author Michal Burda
#' @export
#' @importFrom stats kruskal.test
#' @importFrom rstatix kruskal_effsize
#' @importFrom rstatix dunn_test
report_oneway <- function(data,
                          var,
                          group,
                          omit_levels = NULL,
                          type = c("robust", "parametric", "test"),
                          boxplot = TRUE,
                          digits = 2,
                          thresh = 0.1) {
    type = match.arg(type)

    .must_be_data_frame(data)
    .must_be_character_scalar(var)
    .must_be_character_scalar(group)
    .must_be_character_vector(omit_levels, null = TRUE)

    .must_be_factor(data[[group]], name = "data[[group]]")

    data <- data[!is.na(data[[group]]) & !is.na(data[[var]]), ]
    if (!is.null(omit_levels)) {
        data <- data[!(data[[group]] %in% omit_levels), ]
    }

    res <- list()

    if (boxplot == TRUE) {
        res[["boxplot"]] <- ggplot(data) +
            aes(x = .data[[group]], y = .data[[var]]) +
            geom_boxplot() +
            geom_jitter(width = 0.1, color = "gray", alpha = 0.3) +
            coord_flip() +
            xlab("") + ylab("") +
            theme(legend.position = "none")
    }

    f <- as.formula(paste(var, '~', group))

    if (type == "robust") {
        fit <- kruskal.test(formula = f, data = data)
        eff <- kruskal_effsize(data = data, formula = f)
        tab <- dunn_test(data = data, formula = f, detailed = TRUE)
        tab$effect <- abs(tab$statistic) / sqrt(tab$n1 + tab$n2)
        tab$effect = ifelse(tab$effect < 0.3, "small",
                            ifelse(tab$effect >= 0.5, "large", "moderate"))
        tab$statistic <- format_number(tab$statistic, digits = digits)
        tab$`mean rank diff` = format_number(tab$estimate, digits = digits)
        tab$p <- format_pvalue(tab$p, thresh = thresh)
        tab$p.adj <- format_pvalue(tab$p.adj)
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
        res[["multicomp_title"]] <- paragraph("Dunn's test:")
        res[["multicomp"]] <- kable(tab)
    } else {
        stop("not yet implemented")
    }

    report(res)
}
