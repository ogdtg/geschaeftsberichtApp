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
    tabBox(
      title = "Datenansicht", width = 12,
      tabPanel(
        title = "Tabelle",
        div(
          style = "overflow-x: auto; overflow-y: auto; max-height: 500px;",
          uiOutput(ns("flextable_out"))
        )
      ),
      tabPanel(
        title = "Gesamte Zeitreihe",
        DT::DTOutput(ns("table_out"))
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

    flextable_content <- reactive({
      req(input_values$dept(), input_values$amt(), input_values$table(),input_values$year(),selected_table())
      produce_flextable2(elem=selected_table(),year = input_values$year())

    })

    output$table_out <- DT::renderDT({
      DT::datatable(
        selected_table()[["data"]],
        options = list(scrollX = TRUE, scrollY = "500px", dom = 't', paging = FALSE)
      )
    })

    output$flextable_out <- renderUI({
      req(input_values$year())
      flextable_content()$ft %>%
        htmltools_value()
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
