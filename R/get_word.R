#' @author Min Zhu
#' @title get_word function for Feature 5
#' @description get the word counts from a column of database
#' @param x the database table
#' @return word counts of the database table
#' @references https://shiny.posit.co/r/gallery/start-simple/word-cloud/
#' @import shiny
#' @import tm
#' @importFrom wordcloud wordcloud
#' @importFrom shiny isolate
#' @importFrom tm VectorSource Corpus tm_map TermDocumentMatrix content_transformer stopwords removePunctuation removeNumbers removeWords
#' @importFrom dplyr rename select mutate filter left_join collect
#' @export
get_word <- function(x){
  data("conditions")
  isolate({
    condition_results = left_join(
      x |>
        select(nct_id),
      conditions |>
        select(nct_id, name) |>
        collect(),
      by = "nct_id")

    condition_results = condition_results$name |>
      VectorSource() |>
      Corpus() |>
      tm_map(content_transformer(tolower)) |>
      tm_map(removePunctuation) |>
      tm_map(removeNumbers) |>
      tm_map(removeWords, c(stopwords("SMART"), "the", "and", "but")) |>
      TermDocumentMatrix(control = list(minWordLength = 1)) |>
      as.matrix() |>
      rowSums() |>
      sort(decreasing = TRUE)
  })
}
