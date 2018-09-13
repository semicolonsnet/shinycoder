#' @export
shinycoder <- function() {
  shiny::runApp(system.file(package='shinycoder', 'shinycoder'), display.mode = "normal")
}