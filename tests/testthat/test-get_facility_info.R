test_that("get_facility_info() works", {
  data("facilities")
  data("facility_investigators")
  data("facility_contacts")
  expect_snapshot(get_facility_info(facilities, facility_investigators, facility_contacts))
})
