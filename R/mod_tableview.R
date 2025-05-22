#' Table View UI Module
#'
#' Constructs the user interface for the table viewing module, including a tabbed layout
#' for displaying both a formatted table (`flextable`) and a raw data table (via DT).
#'
#' @param id A unique identifier for the module namespace.
#'
#' @return A UI definition (`tagList`) for use in a Shiny app.
#'
#' @importFrom shiny NS tagList uiOutput downloadButton
#' @importFrom bs4Dash tabBox
#' @importFrom shiny tabPanel
#' @importFrom DT DTOutput
#' @export
mod_tableview_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("title_div")),
    tabBox(
      title = NULL, width = 12,collapsible = F,
      # uiOutput(ns("title_div")),
      tabPanel(
        title = "Tabelle",
        div(
          style = "overflow-x: auto; overflow-y: auto; max-height: 500px;",
          uiOutput(ns("flextable_out")),
          downloadButton(ns("download_table"), "Download"),

        )
      ),
      tabPanel(
        title = "Gesamte Zeitreihe",
        DT::DTOutput(ns("table_out")),
        downloadButton(ns("download_timeseries_csv"), "CSV"),
        downloadButton(ns("download_timeseries_excel"), "Excel"),
        downloadButton(ns("download_timeseries_json"), "JSON")


      )
    )
  )
}

#' Table View Server Module
#'
#' Manages server-side logic for the table viewing module. It handles:
#' - Selection and rendering of a specific dataset from nested input
#' - Rendering of a `flextable` and a `DT` table
#' - Download functionality for the dataset in multiple formats
#'
#' @param id A unique identifier for the module namespace.
#' @param input_values A reactive list of user inputs required to fetch and display data
#'   (`dept`, `amt`, `table`, `year`, `download_id`).
#' @param nested_data A reactive object representing a nested list structure of data.
#'
#' @importFrom shiny moduleServer reactive renderUI req
#' @importFrom writexl write_xlsx
#' @importFrom DT renderDT datatable
#' @importFrom flextable htmltools_value
#' @export
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
        writexl::write_xlsx(list(Sheet1 = flextable_content()$data), path = file)
      }
    )

    output$title_div <-      renderUI({
      div(
        style = "margin-bottom: 10px;",
        p(style = "font-size: 0.9em; margin: 0;", input_values$dep()),
        p(style = "font-size: 0.9em; margin: 0;", input_values$amt()),
        p(style = "font-size: 1.2em; font-weight: bold; margin-top: 5px;", paste0(input_values$table(), " (", input_values$year(), ")"))
      )
    })

  })
}

## To be copied in the UI
# mod_tableview_ui("tableview_1")

## To be copied in the server
# mod_tableview_server("tableview_1")
