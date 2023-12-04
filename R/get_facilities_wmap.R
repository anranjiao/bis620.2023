#' @author Anran Jiao
#' @title The world map of the facilities
#' @description
#' Feature 2: Create a world map of the number of facilities
#' @param facilities the facilities
#' @return A table of the number of facilities joined with world map data
# Reference: https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
#' @importFrom dplyr mutate select group_by summarize left_join collect n
#' @importFrom ggplot2 map_data fortify
#' @export
get_facilities_wmap = function(facilities) {
  facilities_map = facilities |>
    mutate(facility_id = id) |>
    select(facility_id, status, name,  country, state) |>
    group_by(country) |>
    summarize(num = n())

  wmap = ggplot2::map_data('world')
  wmap = fortify(wmap) |>
    mutate(country = region) |>
    select(long, lat, group, country)

  # facilities |> distinct(country) |> summarise(n = n()) # 223
  # wmap |> distinct(country) |> summarise(n = n()) # 252

  # change the names in wmap
  # > "Congo, The Democratic Republic of the" %in% wmap$name
  # [1] FALSE
  # > "Democratic Republic of the Congo" %in% wmap$name
  # [1] TRUE
  original = c("Democratic Republic of the Congo", "Ivory Coast", "Iran", "North Korea", "South Korea",
               "Laos", "Moldova", "Palestine",  "UK", "USA", "Brunei", "Sint Maarten", "Russia", "Syria",
               "Tobago", "Antigua", "Barbuda", "Nevis","Saint Kitts", "Libya","Virgin Islands", "Vatican",
               "Micronesia", "Grenadines","Saint Vincent", "French Southern and Antarctic Lands")
  new = c("Congo, The Democratic Republic of the", "Iran, Islamic Republic of",
          "Korea, Democratic People's Republic of", "Korea, Republic of", "Lao People's Democratic Republic",
          "Moldova, Republic of", "Palestinian Territory, occupied", "United Kingdom", "United States", "Brunei Darussalam",
          "Saint Martin", "Russian Federation", "Syrian Arab Republic","Trinidad and Tobago", "Antigua and Barbuda",
          "Antigua and Barbuda", "Saint Kitts and Nevis","Saint Kitts and Nevis", "Libyan Arab Jamahiriya",
          "Virgin Islands (U.S.)", "Holy See (Vatican City State)", "Federated States of Micronesia",
          "Saint Vincent and the Grenadines", "Saint Vincent and the Grenadines", "Antarctic")

  for (i in 1:length(original)){
    wmap$country[wmap$country == original[i]] <- new[i]
  }

  facilities_wmap = facilities_map |>
    collect() |>
    left_join(wmap, by = "country")
  facilities_wmap$num = log10(facilities_wmap$num)
  return(facilities_wmap)
}
