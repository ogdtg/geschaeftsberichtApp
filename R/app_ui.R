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
      title = "Gesch\u00E4ftsbericht",
      header = init_header("Gesch\u00E4ftsbericht", reference = 'https://statistik.tg.ch'),
      sidebar = dashboardSidebar(mod_sidebar_ui("sidebar_1"),      minified = F),
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
    favicon(ico = "favicon"),
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


#' Initialize Dashboard Header
#'
#' Creates a customized `bs4Dash` dashboard header with a brand title and logo linking to a reference URL.
#'
#' @param dashboard_title A string specifying the title to be displayed in the dashboard header.
#' @param reference A string URL pointing to the website to be linked from the brand logo. Defaults to Thurgau Statistics homepage.
#'
#' @return A `bs4Dash::dashboardHeader` object to be used in a Shiny UI layout.
#'
#' @importFrom bs4Dash dashboardHeader dashboardBrand
#' @importFrom shiny tags a img
#' @export
init_header <- function(dashboard_title,reference='https://statistik.tg.ch'){
  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = dashboard_title,
      href = reference,
    ),
    tags$li(
      a(
        href = reference,
        img(
          src = 'https://www.tg.ch/public/upload/assets/20/logo-kanton-thurgau.svg',
          title = "Company Home",
          height = "30px",
          class = "logoTg"
        ),
        style = "padding-top:10px; padding-bottom:10px;"
      ),
      class = "dropdown"
    )
  )
}


#' Initialize Dashboard Sidebar
#'
#' Constructs the sidebar for a `bs4Dash` dashboard using the provided content. Sidebar is not minimized by default.
#'
#' @param sidebar_content UI elements to be included within the sidebar, such as menu items or input controls.
#'
#' @return A `bs4Dash::dashboardSidebar` object.
#'
#' @importFrom bs4Dash dashboardSidebar
#' @export
init_sidebar <- function(sidebar_content){
  bs4Dash::dashboardSidebar(
    minified = F,
    sidebar_content
  )
}

#' Initialize Dashboard Body
#'
#' Creates the body section of a `bs4Dash` dashboard. This function sets padding, includes custom CSS,
#' initializes `shinyjs` and `shinybrowser`, and adds JavaScript for user tracking and click event handling.
#'
#' @param db_content UI components to be displayed in the body section of the dashboard.
#'
#' @return A `bs4Dash::dashboardBody` object.
#'
#' @importFrom bs4Dash dashboardBody
#' @importFrom shiny tags includeCSS HTML
#' @importFrom shinyjs useShinyjs
#' @importFrom shinybrowser detect
#' @export
init_body <- function(db_content){
  bs4Dash::dashboardBody(
    style = "padding: 0px;",  # Entfernt den Abstand
    useShinyjs(),  # Initialize shinyjs
    shinybrowser::detect(),
    tags$head(
      includeCSS("www/dashboard_style.css")  # Make sure the path to your CSS is correct
    ),
    HTML('<script src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"></script>'),
    tags$script(HTML(
      '
      $(document).on("shiny:connected", function(){
        var newUser = Cookies.get("new_user");
        if(newUser === "false") return;
        Shiny.setInputValue("new_user", true);
        Cookies.set("new_user", false);
      });
      $(document).on("click", ".clickable-element", function() {
        var clicked_id = $(this).attr("id");
        Shiny.setInputValue("clicked_element_id", clicked_id, {priority: "event"});
      });
      $("body").addClass("fixed");
      '
    )),
    db_content
  )
}
