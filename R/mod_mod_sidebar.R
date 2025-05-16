#' mod_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mod_sidebar Server Functions
#'
#' @noRd 
mod_mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_mod_sidebar_ui("mod_sidebar_1")
    
## To be copied in the server
# mod_mod_sidebar_server("mod_sidebar_1")
