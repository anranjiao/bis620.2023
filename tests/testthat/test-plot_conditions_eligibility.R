test_that("plot_conditions_eligibility() works", {
  data("eligibilities")
  vdiffr::expect_doppelganger(
    "plot-conditions-eligibility",
    plot_conditions_eligibility(eligibilities)
  )
})
