#' @author Jonathan Lee
#' @title Query a studies  for the selected eligibility criteria
#' @description Shows a table of studies that aligns with the participant type selected
#' @param d the database table.
#' @importFrom dplyr select rename
#' @export
create_data_table = function(d) {
  d |>
    select(nct_id, gender, minimum_age, maximum_age, population, criteria) |>
    rename(`NCT ID` = nct_id)
}
