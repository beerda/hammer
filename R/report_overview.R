#"
#" @return
#" @author Michal Burda
#" @export
report_overview <- function(.data) {
    .must_be_data_frame(.data)

    m <- as.matrix(.data)
    rows <- tibble()

    new_row(rows) <- data.frame(name = "rows:",
                             value = nrow(m),
                             details = "")

    temp <- apply(m, 1, function(x) all(!is.na(x)))
    det <- format_enumeration(seq_along(temp)[temp])
    new_row(rows) <- data.frame(name = "\u2022 fully specified",
                             value = sum(temp),
                             details = det)

    temp <- apply(m, 1, function(x) any(is.na(x)) && !all(is.na(x)))
    det <- format_enumeration(seq_along(temp)[temp])
    new_row(rows) <- data.frame(name = "\u2022 partially N/A",
                             value = sum(temp),
                             details = det)

    temp <- apply(m, 1, function(x) all(is.na(x)))
    det <- format_enumeration(seq_along(temp)[temp])
    new_row(rows) <- data.frame(name = "\u2022 fully N/A",
                             value = sum(temp),
                             details = det)

    rows$graph <- format_bars(p = rows$value, n = nrow(m))
    rows$value <- format_count_percent(p = rows$value, n = nrow(m), ignore_na = TRUE)

    cols <- data.frame()

    new_row(cols) <- data.frame(name = "columns:",
                             value = ncol(m),
                             details = "")

    temp <- apply(.data, 2, function(x) length(unique(x)) == 1)
    det <- format_enumeration(colnames(m)[temp])
    new_row(cols) <- data.frame(name = "\u2022 constant",
                             value = sum(temp),
                             details = det)

    temp <- apply(.data, 2, function(x) all(!is.na(x)))
    det <- format_enumeration(colnames(m)[temp])
    new_row(cols) <- data.frame(name = "\u2022 fully specified",
                             value = sum(temp),
                             details = det)

    temp <- apply(.data, 2, function(x) any(is.na(x)) && !all(is.na(x)))
    det <- format_enumeration(colnames(m)[temp])
    new_row(cols) <- data.frame(name = "\u2022 partially N/A",
                             value = sum(temp),
                             details = det)

    temp <- apply(.data, 2, function(x) all(is.na(x)))
    det <- format_enumeration(colnames(m)[temp])
    new_row(cols) <- data.frame(name = "\u2022 fully N/A",
                             value = sum(temp),
                             details = det)

    cols$graph <- format_bars(p = cols$value, n = ncol(.data))
    cols$value <- format_count_percent(p = cols$value, n = ncol(.data), ignore_na = TRUE)

    res <- rbind(rows, cols)
    res <- res[, c("name", "value", "graph", "details")]

    res
}
