test_that("create_data_table() works", {
  data("eligibilities")
  expect_snapshot(create_data_table(eligibilities))
})
