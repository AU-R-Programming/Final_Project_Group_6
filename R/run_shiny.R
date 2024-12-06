#' @export
graph_ci = function(){
  appDir = system.file("graphci", package = "FinalProject6")
  shiny::runApp(appDir, display.mode = "normal")
}

