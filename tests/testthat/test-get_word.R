test_that("get_word() works", {
  data("studies")
  data_Feature_5 = studies |> head(1000)

  word_results = get_word(data_Feature_5)

  vdiffr::expect_doppelganger(
    "plot-get_word-1",
    wordcloud(names(word_results),
              word_results,
              scale=c(5,1),
              colors=brewer.pal(7,"BrBG"),
              max.words=20,
              min.freq = 5,)
    )

})
