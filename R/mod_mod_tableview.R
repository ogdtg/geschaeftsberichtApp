#' mod_tableview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_tableview_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mod_tableview Server Functions
#'
#' @noRd 
mod_mod_tableview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mod_tableview_ui("mod_tableview_1")
    
## To be copied in the server
# mod_mod_tableview_server("mod_tableview_1")
