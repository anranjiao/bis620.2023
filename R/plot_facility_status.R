#' @author Anran Jiao
#' @title Plot the status of the facilities
#' @param d the database table.
#' @description
#' Plot the number of facilities with different status
#' @importFrom dplyr group_by summarize select n
#' @importFrom utils head
#' @importFrom ggplot2 ggplot geom_col xlab ylab theme_bw aes scale_x_discrete
#' @export
plot_facility_status = function(d) {
  d = d |>
    select(status) |>
    group_by(status) |>
    summarize(n = n())

  fixed_x_labels <- c("Recruiting", "Not yet recruiting", "Available")

  ggplot(d, aes(x = factor(status, levels = fixed_x_labels), y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Status") +
    ylab("Count") +
    scale_x_discrete(labels = fixed_x_labels)
}
