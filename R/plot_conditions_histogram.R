#' @author Min Zhu
#' @title The condition histogram of Problem 2
#' @description Create a histogram of the conditions returned by a brief title keyword search
#' @param x the database table.
#' @param brief_title_kw the brief title keywords to look for. This is optional.
#' @return a histogram of the conditions
#' @importFrom dplyr group_by summarize arrange select mutate n desc
#' @importFrom utils head
#' @importFrom ggplot2 ggplot geom_col xlab ylab theme aes theme_bw element_text
#' @export
plot_conditions_histogram = function(x) {
  x$conditions[is.na(x$conditions)] = "NA"
  x = x |>
    select(conditions) |>
    group_by(conditions) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    mutate(conditions = factor(conditions, levels = conditions)) |>
    head(20)

  ggplot(x, aes(x = conditions, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Conditions") +
    ylab("Count")
}
