#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  nested_data <- reactive(produce_data_list())
  input_values <- mod_sidebar_server("sidebar_1", nested_data)
  mod_tableview_server("tableview_1", input_values, nested_data)
}
