#' Run the NBA Shiny entry point
#'
#' @param ... Passed to [shiny::shinyApp()].
#' @export
run_app <- function(...) {
  shiny::shinyApp(ui = app_ui, server = app_server, ...)
}
