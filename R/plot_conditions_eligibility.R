#' @author Jonathan Lee
#' @title Plot a histogram of the count of conditions investigated for the selected eligibility criteria
#' @description Shows a number of conditions studied in a descending order
#' @param d the database table.
#' @importFrom dplyr group_by summarize arrange n desc
#' @importFrom utils head
#' @importFrom ggplot2 ggplot geom_col xlab ylab theme aes element_text
#' @export
plot_conditions_eligibility = function(d) {
  d |>
    group_by(name) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    head(10) |>
    ggplot(aes(x = reorder(name, -n), y = n)) +
    geom_col() +
    xlab("Conditions") +
    ylab("Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
