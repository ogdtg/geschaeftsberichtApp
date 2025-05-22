test_input_values <- reactiveValues(
  dept = reactive("DJS"),
  amt = reactive("Jagd- und Fischereiverwaltung"),
  table = reactive("Fischeinsätze in anderen Gewässern"),
  year = reactive(2024),
  download_id = "download_table",
  dep = reactive("DJS")
)

# Dummy nested data
dummy_data <- reactive({
  list(
    DJS = list(
      `Jagd- und Fischereiverwaltung` = list(
        `Fischeinsätze in anderen Gewässern` = list(
          table_type = "nested_col",
          group_col = NULL,
          label_col = NULL,
          colnames_col = "gewaesser",
          val_col = "anzahl",
          new_names = NULL,
          level1_col = "fischart_kennzahl",
          level1_name = "Fischart",
          level2_col = "fischeinsatz_typ",
          level2_name = "Einsatz",
          id_col = NULL,
          total_row = NULL,
          total_col = NULL,
          turn_header = NULL,
          inner_border = NULL,
          data = data.frame(
            jahr = c("2024", "2024"),
            fischart_kennzahl = c("Bachforellen", "Bachforellen"),
            fischeinsatz_typ = c("Brütlinge angefüttert", "Brütlinge angefüttert"),
            gewaesser = c("Aufzuchtgewässer", "Rhein"),
            anzahl = c(210000, NA)
          )
        )
      )
    )
  )
})


test_that("mod_tableview_ui returns a tagList", {
  ui <- mod_tableview_ui("test_ui")
  expect_s3_class(ui, "shiny.tag.list")
})


test_that("mod_tableview_server selects correct table", {
  testServer(mod_tableview_server, args = list(
    input_values = test_input_values,
    nested_data = dummy_data
  ), {
    expect_equal(selected_table(), dummy_data()[["DJS"]][["Jagd- und Fischereiverwaltung"]][["Fischeinsätze in anderen Gewässern"]])
  })
})

test_that("flextable_content is reactive and returns list", {
  testServer(mod_tableview_server, args = list(
    input_values = test_input_values,
    nested_data = dummy_data
  ), {
    # print(flextable_content())
    expect_type(flextable_content(), "list")
    expect_true("ft" %in% names(flextable_content()))
    expect_true("data" %in% names(flextable_content()))
  })
})


