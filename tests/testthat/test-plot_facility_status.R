test_that("plot_facility_status() works", {
  data("facilities")
  data("facility_investigators")
  data("facility_contacts")
  facility_info = get_facility_info(facilities, facility_investigators, facility_contacts)
  vdiffr::expect_doppelganger(
    "plot-facility-status",
    plot_facility_status(facility_info)
  )
})
