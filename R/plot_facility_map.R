#' @author Anran Jiao
#' @title Plot the world map of the facilities
#' @description
#' Plot a world map with the number of facilities
#' @param d the database table.
#' @importFrom dplyr group_by summarize select mutate
#' @importFrom utils head
#' @importFrom ggplot2 ggplot geom_col aes xlab ylab theme theme_bw aes element_blank element_rect scale_x_discrete scale_fill_gradientn scale_x_continuous scale_y_continuous
#' @importFrom ggiraph geom_polygon_interactive
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_facility_map = function(d) {
  d = d |> mutate(`log10(#Facilities)` = num)
  ggplot() +
    geom_polygon_interactive(data = subset(d, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = `log10(#Facilities)` , group = group,
                                 tooltip = sprintf("%s<br/>%s", country, 10^num))) +
    scale_fill_gradientn(colours = brewer.pal(8, "PuBuGn"), na.value = 'gray50') +
    scale_y_continuous(limits = c(-60, 90), breaks = c()) +
    scale_x_continuous(breaks = c()) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(fill = 'white', colour = 'white'))
}
