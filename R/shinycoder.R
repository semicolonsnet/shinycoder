#' @export
shinycoder <- function() {
  shiny::runApp(file.path('inst', package='shinycoder'), display.mode = "normal")
}