#' Sidebar UI Module
#'
#' Provides a user interface for selecting a department, unit, table, and year from hierarchical data.
#' The selections are dynamically updated based on previous input choices. Includes a download button for the final table.
#'
#' @param id A unique identifier for the namespace of the module.
#'
#' @return A `shiny::tagList` of UI elements to include in a Shiny app.
#'
#' @importFrom shiny NS tagList selectizeInput downloadButton
#' @noRd
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("dept"), "Departement", choices = NULL, selected = NULL,
                   options = list(placeholder = 'Departement w\u00E4hlen', allowEmptyOption = TRUE)),
    selectizeInput(ns("amt"), "Amt", choices = NULL),
    selectizeInput(ns("table"), "Tabelle", choices = NULL),
    selectizeInput(ns("year"), "Jahr", choices = NULL),
    downloadButton(ns("download"), "Download Table (Excel)")
  )
}


#' Sidebar Server Module
#'
#' Implements the server-side logic for cascading select inputs (department → unit → table → year).
#' Inputs are updated based on available options in the `nested_data` reactive object.
#' It also supports initializing selections from URL query parameters and dynamically updates the browser URL using `runjs()`.
#'
#' @param id Module namespace ID.
#' @param nested_data A reactive function returning a nested list of data: `nested_data()[[dept]][[amt]][[table]]`.
#'
#' @return A list of reactive expressions:
#' \describe{
#'   \item{dept}{Selected department}
#'   \item{amt}{Selected unit}
#'   \item{table}{Selected table}
#'   \item{year}{Selected year}
#'   \item{download_id}{Namespace ID for the download button}
#' }
#'
#' @importFrom shiny moduleServer reactiveValues reactiveVal observe observeEvent req parseQueryString updateSelectizeInput
#' @importFrom shinyjs runjs
#' @importFrom dplyr filter pull

#'
#' @noRd
mod_sidebar_server <- function(id, nested_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Query handling and cascading input logic
    query_vals <- reactiveValues(dept = NULL,amt = NULL,table = NULL, year = NULL)
    query_done <- reactiveVal(FALSE)

    observe({
      if (query_done()) return()
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query$id) && query$id %in% table_list$id) {
        temp <- table_list %>% filter(id == query$id)

        query_vals$dept <- temp$dep
        query_vals$amt <- temp$amt
        query_vals$table <- temp$table
        query_vals$year <- query$year
      }

      query_done(TRUE)
    })

    observe({

      query <- parseQueryString(session$clientData$url_search)

      if (!is.null(query$id) && query$id %in% table_list$id) {
        # print(query)
        temp <- table_list %>%
          filter(id == query$id)

        amt_choices <- table_list %>%
          filter(dep == temp$dep) %>%
          pull(amt)

        table_choices <- table_list %>%
          filter(amt == temp$amt) %>%
          pull(table)

        year_choices <- data_list[[query$id]]$jahr %>% unique() %>% sort(decreasing = TRUE)

        updateSelectizeInput(session, "dept", selected = query$dept, server = TRUE)
        updateSelectizeInput(session, "amt", choices = amt_choices, selected = temp$amt, server = TRUE)
        updateSelectizeInput(session, "table", choices = table_choices, selected = temp$table, server = TRUE)
        updateSelectizeInput(session, "year", choices = year_choices, selected = query$year, server = TRUE)
      }



      # dept_choices <- names(nested_data_ogd)
      # if (!is.null(query$dept) && query$dept %in% dept_choices) {
      #   amt_choices <- names(nested_data_ogd[[query$dept]])
      #   updateSelectizeInput(session, "amt", choices = amt_choices, selected = query$amt, server = TRUE)
      #
      #   if (!is.null(query$amt) && query$amt %in% amt_choices) {
      #     table_choices <- names(nested_data_ogd[[query$dept]][[query$amt]])
      #     updateSelectizeInput(session, "table", choices = table_choices, selected = query$table, server = TRUE)
      #
      #     if (!is.null(query$table) && query$table %in% table_choices) {
      #       year_choices <- unique(nested_data_ogd[[query$dept]][[query$amt]][[query$table]][["data"]][["jahr"]]) %>% sort(decreasing = TRUE)
      #       updateSelectizeInput(session, "year", choices =year_choices , selected = query$year, server = TRUE)
      #     }
      #   }
      # }
    })

    observe({


      updateSelectizeInput(session, "dept", choices = unique(table_list$dep))
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


      valid_years <- unique(nested_data()[[input$dept]][[input$amt]][[input$table]][["data"]][["jahr"]]) %>% sort(decreasing = TRUE)
      selected_year <- if (!is.null(query_vals$year) && query_vals$year %in% valid_years) query_vals$year else valid_years[1]
      updateSelectizeInput(session, "year", choices = valid_years, selected = selected_year)
      query_vals$year <- NULL
    }, ignoreInit = TRUE)

    observe({
      req(input$dept, input$amt, input$table,input$year)

      # url_query <- paste0(
      #   # "?ogd=", URLencode(input$ogd),
      #   "?dept=", URLencode(input$dept),
      #   "&amt=", URLencode(input$amt),
      #   "&table=", URLencode(input$table),
      #   "&year=", URLencode(input$year)
      #   # if (input$ogd == "Normal") paste0("&year=", URLencode(input$year)) else ""
      # )

      temp_id <- table_list %>%
        filter(dep == input$dept) %>%
        filter(amt == input$amt) %>%
        filter(table == input$table) %>%
        pull(id)

      url_query <- paste0(
        # "?ogd=", URLencode(input$ogd),
        # "?dept=", URLencode(input$dept),
        # "&amt=", URLencode(input$amt),
        "?id=", URLencode(temp_id),
        "&year=", URLencode(input$year)
        # if (input$ogd == "Normal") paste0("&year=", URLencode(input$year)) else ""
      )
      runjs(sprintf("history.replaceState(null, '', '%s')", url_query))
    })

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
