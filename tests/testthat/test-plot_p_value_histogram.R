test_that("plot_p_value_histogram() works", {
  data("outcomes")
  data("outcome_analyses")
  data_Feature_6 = left_join(
    outcomes, outcome_analyses |>
      filter(!is.na(p_value)), by="nct_id") |>
    head(1000) |>
    # collect() |>
    select(nct_id, description, p_value, p_value_description)

  data_Feature_6 = data_Feature_6 |>
    mutate(p_value_range = case_when(
      p_value <= 0.001 ~ "p value <= 0.001",
      p_value <= 0.01 ~ "0.001 < p value <= 0.01",
      p_value <= 0.05 ~ "0.01 < p value <= 0.05",
      p_value <= 0.1 ~ "0.05 < p value <= 0.1",
      p_value <= 0.5 ~ "0.1 < p value <= 0.5",
      p_value <= 1.0 ~ "0.5 < p value <= 1.0",
      .default = "p value > 1.0"
    ))
  vdiffr::expect_doppelganger(
    "plot_p_value_histogram-1",
    plot_p_value_histogram(data_Feature_6)
  )
})
