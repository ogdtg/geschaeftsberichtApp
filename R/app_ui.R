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
      sidebar = dashboardSidebar(mod_sidebar_ui("sidebar_1"),      minified = F),
      body = dashboardBody(
        shinyjs::useShinyjs(),
        tags$style(HTML(".wrapper .content-wrapper {
        margin-top: 30px !important;
        }
/* Active tab */
.nav-pills .nav-link.active,
.nav-pills .show > .nav-link {
  color: #fff !important;
  background-color: #367d32 !important;
}

/* Hover only on non-active tabs */
.nav-pills .nav-link:not(.active):hover {
  color: #367d32 !important;
}



        ")),
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
    # Add your custom CSS link here
    # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
}
