#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bs4Dash::dashboardPage(
      title = "Geschäftsbericht",
      header = init_header("Geschäftsbericht", reference = 'https://statistik.tg.ch'),
      sidebar = dashboardSidebar(mod_sidebar_ui("sidebar_1")),
      body = dashboardBody(
        shinyjs::useShinyjs(),
        mod_tableview_ui("tableview_1")
      )
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "geschaeftsberichtApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
