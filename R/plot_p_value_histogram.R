#' @author Min Zhu
#' @title The p value distribution histogram of Feature 6
#' @description Create a histogram of the p value distribution returned by a table of p value
#' @param x the p value table.
#' @return a histogram of p value distribution
#' @importFrom dplyr group_by summarize arrange select mutate n desc case_when
#' @importFrom utils head
#' @importFrom ggplot2 ggplot geom_col xlab ylab theme aes theme_bw element_text
#' @export
plot_p_value_histogram = function(x) {
  x = x |>
    select(p_value_range) |>
    group_by(p_value_range) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    mutate(p_value_range = factor(p_value_range, levels = p_value_range))

  ggplot(x, aes(x = p_value_range, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("p Value Range") +
    ylab("Count")
}
