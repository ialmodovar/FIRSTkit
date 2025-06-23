#' Launch the FIRSTkit Shiny App
#'
#' This function launches the FIRSTkit app.
#' @export
FIRSTkit <- function() {
  shiny::shinyApp(ui = app_ui(), server = app_server)
}
