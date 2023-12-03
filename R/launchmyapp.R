launchmyapp <- function() {
  appdir <- system.file("shinyapp", package = "bis620.2023")
  if (appdir == "") {
    stop("Shiny app not found in the package")
  }
  shiny::runApp(appdir)
}
