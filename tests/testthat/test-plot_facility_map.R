test_that("plot_facility_map() works", {
  data("facilities")
  facilities_wmap = get_facilities_wmap(facilities)
  vdiffr::expect_doppelganger(
    "plot-facility-map",
    plot_facility_map(facilities_wmap)
  )
})
