library(shiny)
library(shinyjs)
library(bs4Dash)
library(waiter)

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


init_sidebar <- function(sidebar_content){
  bs4Dash::dashboardSidebar(
    minified = F,
    sidebar_content
  )
}


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

