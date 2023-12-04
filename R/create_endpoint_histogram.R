#' @title Create a histogram of the endpoint
#' @importFrom dplyr summarize group_by select n collect left_join
#' @importFrom ggplot2 ggplot aes geom_col theme_bw scale_y_log10
#' @param studies the studies database table.
#' @param endpoints the endpoints database table.
#' @param kw the keyword.
#' @export
create_endpoint_histogram = function(studies, endpoints, kw) {
  em = query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    select(nct_id) |>
    collect() |>
    left_join(endpoints, by = "nct_id") |>
    group_by(endpoint_met) |>
    summarize(n = n())

  ggplot(em, aes(x = endpoint_met, y = n)) +
    geom_col() +
    scale_y_log10() +
    theme_bw()
}
