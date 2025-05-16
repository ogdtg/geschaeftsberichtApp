#' tableview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tableview_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::tabBox(
      title = "Datenansicht", width = 12,
      bs4Dash::tabPanel(
        title = "Tabelle",
        div(
          style = "overflow-x: auto; overflow-y: auto; max-height: 500px;",
          uiOutput(ns("flextable_out"))
        )
      ),
      tabPanel(
        title = "Gesamte Zeitreihe",
        dataTableOutput(ns("table_out"))
      )
    )
  )
}

#' tableview Server Functions
#'
#' @noRd
mod_tableview_server <- function(id, input_values, nested_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_table <- reactive({
      req(input_values$dept(), input_values$amt(), input_values$table())
      nested_data()[[input_values$dept()]][[input_values$amt()]][[input_values$table()]]
    })

    output$table_out <- renderDataTable({
      datatable(
        selected_table()[["data"]],
        options = list(scrollX = TRUE, scrollY = "500px", dom = 't', paging = FALSE)
      )
    })

    output$flextable_out <- renderUI({
      req(input_values$year())
      produce_flextable2(selected_table(), input_values$year())$ft %>% htmltools_value()
    })

    output[[input_values$download_id]] <- downloadHandler(
      filename = function() {
        paste0(gsub(" ", "_", input_values$table()), "_", input_values$year(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(list(Sheet1 = produce_flextable2(selected_table(), input_values$year())$data), path = file)
      }
    )
  })
}

## To be copied in the UI
# mod_tableview_ui("tableview_1")

## To be copied in the server
# mod_tableview_server("tableview_1")
