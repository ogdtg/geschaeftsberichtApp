#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("dept"), "Departement", choices = NULL, selected = NULL,
                   options = list(placeholder = 'Departement wählen', allowEmptyOption = TRUE)),
    selectizeInput(ns("amt"), "Amt", choices = NULL),
    selectizeInput(ns("table"), "Tabelle", choices = NULL),
    selectizeInput(ns("year"), "Jahr", choices = NULL),
    downloadButton(ns("download"), "Download Table (Excel)")
  )
}


#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, nested_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Query handling and cascading input logic
    query_vals <- reactiveValues(dept = NULL, amt = NULL, table = NULL, year = NULL)
    query_done <- reactiveVal(FALSE)

    observe({
      if (query_done()) return()
      query <- parseQueryString(session$clientData$url_search)

      query_vals$dept <- query$dept
      query_vals$amt <- query$amt
      query_vals$table <- query$table
      query_vals$year <- query$year
      query_done(TRUE)
    })

    observe({
      updateSelectizeInput(session, "dept", choices = names(nested_data()))
    })

    observeEvent(input$dept, {
      req(query_done(), input$dept)
      amts <- names(nested_data()[[input$dept]])
      selected_amt <- if (!is.null(query_vals$amt) && query_vals$amt %in% amts) query_vals$amt else amts[1]
      updateSelectizeInput(session, "amt", choices = amts, selected = selected_amt)
      query_vals$amt <- NULL
    }, ignoreInit = TRUE)

    observeEvent(input$amt, {
      req(input$dept, input$amt)
      tbls <- names(nested_data()[[input$dept]][[input$amt]])
      selected_tbl <- if (!is.null(query_vals$table) && query_vals$table %in% tbls) query_vals$table else tbls[1]
      updateSelectizeInput(session, "table", choices = tbls, selected = selected_tbl)
      query_vals$table <- NULL
    }, ignoreInit = TRUE)

    observeEvent(input$table, {
      req(input$dept, input$amt, input$table)
      valid_years <- unique(nested_data()[[input$dept]][[input$amt]][[input$table]][["data"]][["jahr"]])
      selected_year <- if (!is.null(query_vals$year) && query_vals$year %in% valid_years) query_vals$year else valid_years[1]
      updateSelectizeInput(session, "year", choices = valid_years, selected = selected_year)
      query_vals$year <- NULL
    }, ignoreInit = TRUE)

    # Return values to pass to other modules
    return(
      list(
        dept = reactive(input$dept),
        amt = reactive(input$amt),
        table = reactive(input$table),
        year = reactive(input$year),
        download_id = ns("download")
      )
    )
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
