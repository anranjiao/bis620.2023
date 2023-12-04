#' @author Anran Jiao
#' @title The information of facilities, the investigators, and contacts
#' @description
#' Feature 1: Search the facilities, investigators of the facilities, and contacts information and filter
#' the data on status, country, role of investigators.
#' @param facilities facilities
#' @param facility_investigators facility_investigators
#' @param facility_contacts facility_contacts
#' @return A joined table facility_info with investigators of the facilities, and contacts information.
#' @importFrom dplyr rename select mutate filter left_join collect n
#' @export
get_facility_info = function(facilities, facility_investigators, facility_contacts){
  Facilities = facilities |>
    rename(`facility_id` = id) |>
    select(facility_id, nct_id, status, name,  country, state)

  Facility_investigators = facility_investigators |>
    mutate(investigator = name) |>
    select(facility_id, investigator, role)

  Facility_contacts = facility_contacts |>
    mutate(contact = name) |>
    select(facility_id, contact, contact_type, email)

  # Checked the nct_id matches : identical(facility_info[['nct_id.x']],facility_info[['nct_id.y']])
  # keep columns with known status
  facility_info = Facilities |>
    filter(!is.na(status)) |>
    left_join(Facility_investigators, by = "facility_id") |>
    filter(!is.na(investigator)) |>
    left_join(Facility_contacts,  by = "facility_id") |>
    filter(!is.na(contact))
  return(facility_info)
}

