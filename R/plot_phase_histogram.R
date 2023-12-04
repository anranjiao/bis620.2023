#' @title Create a histogram of the phases returned by a brief title keyword search
#' @param x the database table.
#' @importFrom dplyr summarize group_by select n
#' @importFrom ggplot2 ggplot aes geom_col theme_bw xlab ylab scale_x_discrete theme element_text
#' @export
plot_phase_histogram = function(x) {
  x$phase[is.na(x$phase)] = "NA"
  x = x |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())

  fixed_x_labels <- c("NA", "Not Applicable", "Early Phase 1", "Phase 1", "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3", "Phase 3", "Phase 4")

  ggplot(x, aes(x = factor(phase, levels = fixed_x_labels), y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count") +
    scale_x_discrete(labels = fixed_x_labels, drop = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
