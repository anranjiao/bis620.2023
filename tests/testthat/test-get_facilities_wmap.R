test_that("get_facilities_wmap() works", {
  data("facilities")
  expect_snapshot(get_facilities_wmap(facilities))
})
