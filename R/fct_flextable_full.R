


# Utility

#' Create Thin Border Style
#'
#' Creates a thin black border for flextable formatting.
#'
#' @importFrom officer fp_border
#' @return An officer border object with thin black line
#' @export
thin_border <- function() {
  officer::fp_border(color = "black", width = .5)
}

#' Create Fat Border Style
#'
#' Creates a thick black border for flextable formatting.
#'
#' @importFrom officer fp_border
#' @return An officer border object with thick black line
#' @export
fat_border <- function() {
  officer::fp_border(color = "black", width = 1)
}

#' Transform to Wide Format with Nested Headers
#'
#' Converts a long-format dataset into a wide format. If a second header level (`l2`) exists,
#' it combines `l1` and `l2` into a nested header (`nh`). Otherwise, it renames `l1` to `nh`.
#'
#' @param df A data frame in long format with at least the columns `l1` and optionally `l2` and `value`.
#'
#' @return A wide-format data frame with `nh` as column headers.
#' @export
#' @importFrom dplyr mutate select rename
#' @importFrom tidyr pivot_wider
transform_to_wide <- function(df){
  if ("l2" %in% names(df)){
    df <- df %>%
      mutate(nh = paste0(l1,"__",l2)) %>%
      select(-c(l1,l2))
  } else if ("l1" %in% names(df)) {
    df <- df %>%
      rename(nh = "l1")
  } else {
    return(df)
  }
  df %>%
    pivot_wider(names_from = nh, values_from = value)
}


#' Create Subsections with Optional Subtotals
#'
#' Adds nested row groups and optionally calculates subtotals based on grouping variables.
#'
#' @param df A data frame with columns `group`, `label`, and value columns (already in wide format).
#' @param subtotals_in_data Logical, indicating if subtotal rows are already included in the data.
#' @param subtotals Logical, indicating if subtotals should be shown.
#'
#' @return A data frame with `.label` and `total_col` columns for use in `flextable`, including subtotal rows if specified.
#' @export
#' @importFrom dplyr group_by mutate case_when ungroup filter anti_join select relocate rename summarise across bind_rows join_by
#' @importFrom purrr map
create_subsections <- function(df, subtotals_in_data, subtotals) {
  if (isTRUE(subtotals_in_data)){
    subtotals <- TRUE
  }

  no_group_data <- df %>%
    group_by(group) %>%
    mutate(
      .n_group = n(),
      .label = case_when(
        .n_group == 1 & group == label ~ label,
        TRUE ~ paste0("- ", as.character(label))
      )
    ) %>%
    ungroup()

  totals_data <- no_group_data %>%
    filter(.n_group > 1 | group != label)

  no_totals_data <- no_group_data %>%
    anti_join(totals_data, join_by(label, group)) %>%
    select(-c(group, label, .n_group)) %>%
    relocate(.label) %>%
    mutate(total_col = TRUE)

  if (isTRUE(subtotals_in_data)) {
    totals_data_mod <- totals_data %>%
      filter(group == label)

    totals_data <- totals_data %>%
      anti_join(totals_data_mod, join_by(group, label, .label))

    no_group_data <- no_group_data %>%
      anti_join(totals_data_mod, join_by(group, label, .label))
  }

  colnames <- df %>%
    select(-c(group, label)) %>%
    names()

  grouped_df <- lapply(unique(totals_data$group), function(x) {
    if (isTRUE(subtotals_in_data)) {
      temp_total <- totals_data_mod %>%
        filter(group == x) %>%
        mutate(total_col = TRUE) %>%
        select(-c(.n_group, .label, label)) %>%
        rename(.label = group)
    } else {
      temp_total <- totals_data %>%
        filter(group == x) %>%
        group_by(group) %>%
        summarise(across(all_of(colnames), ~ if (is.numeric(.x)) sum(.x, na.rm = TRUE) else ""), .groups = "drop") %>%
        rename(.label = group) %>%
        mutate(total_col = TRUE)
    }

    if (isFALSE(subtotals)) {
      for (col_val in colnames) {
        temp_total[[col_val]] <- NA
      }
    }

    temp <- totals_data %>%
      filter(group == x) %>%
      select(-c(group, .n_group, label))

    bind_rows(temp_total, temp)
  }) %>%
    bind_rows() %>%
    bind_rows(no_totals_data)

  return(grouped_df)
}


#' Add Total Row to a Data Frame
#'
#' Sums numeric columns across all rows and appends the result as a "Total" row.
#'
#' @param df A data frame.
#'
#' @return A data frame with an additional summary row.
#' @export
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate filter bind_rows
#' @importFrom stats setNames
add_total_row <- function(df){

  if ("total_col" %in% names(df)){
    summary_data <- df %>%
      filter(total_col)
  } else {
    summary_data <- df
  }
  summary_row <- summary_data %>%
    purrr::map(~ if (is.numeric(.)) sum(., na.rm = TRUE) else "") %>%
    as.data.frame() %>%
    setNames(names(df)) %>%
    mutate(summary_row = TRUE)
  summary_row[,1] <- "Total"

  if ("total_col" %in% names(df)){
    summary_row$total_col <- as.logical(summary_row$total_col)
  }

  df <- df %>%
    bind_rows(summary_row)
  return(df)
}


#' Add Total Column to a Data Frame
#'
#' Computes row-wise totals of numeric columns and adds a `.total_col` column.
#'
#' @param df A data frame.
#'
#' @return A data frame with a `.total_col` column.
#' @export
add_total_col <- function(df) {
  numeric_cols <- sapply(df, is.numeric)
  df$.total_col <- rowSums(df[, numeric_cols, drop = FALSE], na.rm = TRUE)
  return(df)
}



#' Create Nested Table with Double Column and Header
#'
#' Transforms and organizes data to prepare a double-level header and nested row labels with optional subtotals.
#'
#' @param data A data frame in long format.
#' @param header_cols A character vector of length 2 indicating the header columns (e.g., c("name", "indikator")).
#' @param column_cols A character vector of length 2 indicating the row group and label columns.
#' @param val_col A string naming the column containing the numeric values.
#'
#' @return A renamed data frame
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang sym
template_double_col_double_header <- function(data, header_cols, column_cols, val_col) {
  df <- data %>%
    dplyr::rename(
      group = !!rlang::sym(column_cols[1]),
      label = !!rlang::sym(column_cols[2]),
      l1 = !!rlang::sym(header_cols[1]),
      l2 = !!rlang::sym(header_cols[2]),
      value = !!rlang::sym(val_col)
    )

  return(df)
}







#' Create Nested Table with Single-Level Header
#'
#' Transforms and organizes data to prepare a single-level column header and nested row labels with optional subtotals.
#'
#' @param data A data frame in long format.
#' @param header_cols A character vector of length 1 indicating the header column (e.g., `"indikator"`).
#' @param column_cols A character vector of length 2 indicating the row group and label columns.
#' @param val_col A string naming the column containing the numeric values.
#'
#' @return A renamed data frame
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang sym
template_double_col_single_header <- function(data, header_cols, column_cols, val_col ) {
  df <- data %>%
    dplyr::rename(
      group = !!rlang::sym(column_cols[1]),
      label = !!rlang::sym(column_cols[2]),
      l1 = !!rlang::sym(header_cols[1]),
      value = !!rlang::sym(val_col)
    )

  return(df)
}


#' Create Nested Table with No Header and Two Column Levels
#'
#' Prepares a dataset using group and label columns without header information.
#'
#' @param data A data frame in long format.
#' @param column_cols A character vector of length 2 indicating group and label columns.
#'
#' @return A renamed data frame.
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang sym
template_double_col_no_header <- function(data, column_cols ) {
  df <- data %>%
    dplyr::rename(
      group = !!rlang::sym(column_cols[1]),
      label = !!rlang::sym(column_cols[2])
    )

  return(df)
}

#' Create Nested Table with One Column and Two Header Levels
#'
#' Transforms and organizes data to include a double-level header and a single row label.
#'
#' @param data A data frame in long format.
#' @param header_cols A character vector of length 2 for the header columns.
#' @param column_cols A character vector of length 1 for the row label.
#' @param val_col A string naming the column containing the numeric values.
#'
#' @return A renamed data frame.
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang sym
template_single_col_double_header <- function(data, header_cols, column_cols, val_col ) {
  df <- data %>%
    dplyr::rename(
      .label = !!rlang::sym(column_cols[1]),
      l1 = !!rlang::sym(header_cols[1]),
      l2 = !!rlang::sym(header_cols[2]),
      value = !!rlang::sym(val_col)
    )

  return(df)
}

#' Create Nested Table with One Column and One Header
#'
#' Transforms data with one column and one header into a renamed format.
#'
#' @param data A data frame.
#' @param header_cols A character vector of length 1.
#' @param column_cols A character vector of length 1.
#' @param val_col A string for the value column name.
#'
#' @return A renamed data frame.
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang sym
template_single_col_single_header <- function(data, header_cols, column_cols, val_col ) {
  df <- data %>%
    dplyr::rename(
      .label = !!rlang::sym(column_cols[1]),
      l1 = !!rlang::sym(header_cols[1]),
      value = !!rlang::sym(val_col)
    )

  return(df)
}

#' Create Table with No Column Grouping and Single Header
#'
#' Useful for simple tables with one header and no explicit grouping columns.
#'
#' @param data A data frame.
#' @param header_cols A character vector of length 1.
#' @param column_cols A character vector of length 1 (used for labeling).
#' @param val_col A string for the value column.
#'
#' @return A renamed data frame.
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang sym
template_no_col_single_header <- function(data, header_cols,column_cols, val_col ) {
  df <- data %>%
    dplyr::rename(
      .label = !!rlang::sym(column_cols[1]),
      l1 = !!rlang::sym(header_cols[1]),
      value = !!rlang::sym(val_col)
    )

  return(df)
}

#' Create Table with No Column Grouping and Two Header Levels
#'
#' Useful when your data only has two header levels and a single label.
#'
#' @param data A data frame.
#' @param header_cols A character vector of length 2.
#' @param column_cols A character vector of length 1 (used for labeling).
#' @param val_col A string for the value column.
#'
#' @return A renamed data frame.
#' @export
#' @importFrom dplyr rename
#' @importFrom rlang sym
template_no_col_double_header <- function(data, header_cols,column_cols, val_col ) {
  df <- data %>%
    dplyr::rename(
      .label = !!rlang::sym(column_cols[1]),
      l1 = !!rlang::sym(header_cols[1]),
      l2 = !!rlang::sym(header_cols[2]),
      value = !!rlang::sym(val_col)
    )

  return(df)
}


#' Create Template Data for Flextable Rendering
#'
#' Prepares and structures input data according to header and column configuration,
#' including wide transformation and optional subtotal/total formatting.
#'
#' @param data Input data frame.
#' @param header_cols Character vector. Header column names (1 or 2).
#' @param column_cols Character vector. Row group/label column names (0, 1, or 2).
#' @param val_col String. Column with values to display.
#' @param subtotals Logical. Add subtotals?
#' @param subtotals_in_data Logical. Are subtotals already in the data?
#' @param total_in_data Logical. Is a total row already in the data?
#' @param total_col Logical. Add a total column?
#' @param total_row Logical. Add a total row?
#'
#' @return A data frame ready for flextable formatting.
#' @export
#' @importFrom dplyr rename
create_template_data <- function(data, header_cols, column_cols, subtotals, val_col, subtotals_in_data, total_in_data, total_col, total_row) {

  if (length(header_cols) == 0 & length(column_cols) == 0) {
    return(data)
  }

  if (length(header_cols) == 2 & length(column_cols) == 2) {
    df <- template_double_col_double_header(data, header_cols, column_cols, val_col)
  }
  if (length(header_cols) == 1 & length(column_cols) == 2) {
    df <- template_double_col_single_header(data, header_cols, column_cols, val_col)
  }
  if (length(header_cols) == 2 & length(column_cols) == 1) {
    df <- template_single_col_double_header(data, header_cols, column_cols, val_col)
  }
  if (length(header_cols) == 1 & length(column_cols) == 1) {
    df <- template_single_col_single_header(data, header_cols, column_cols, val_col)
  }
  if (length(header_cols) == 1 & length(column_cols) == 0) {
    df <- template_no_col_single_header(data, header_cols, column_cols = names(data)[1], val_col)
  }
  if (length(header_cols) == 2 & length(column_cols) == 0) {
    df <- template_no_col_double_header(data, header_cols, column_cols = names(data)[1], val_col)
  }
  if (length(header_cols) == 0 & length(column_cols) == 2) {
    df <- template_double_col_no_header(data, column_cols)
  }

  df <- transform_to_wide(df)

  if (length(column_cols) == 2) {
    df <- create_subsections(df, subtotals_in_data, subtotals)
  }

  if (isTRUE(total_row)) {
    df <- add_total_row(df)
  }

  if (isTRUE(total_col)) {
    df <- add_total_col(df)
  }

  if (isTRUE(total_in_data)) {
    df$summary_row <- NA
    df$total_col[nrow(df)] <- NA
    df$summary_row[nrow(df)] <- TRUE
  }

  return(df)
}




# data <- ogd_data_list[["DJS"]][["Amt für Handelsregister und Zivilstandswesen"]][["Ordentliche Einbürgerungen"]][["data"]]


# column_cols <- c("kennwert_unterteilung","kennwert")
# header_cols <- c("jahr")
# val_col <- "anzahl"
# subtotals_in_data <- NULL
# subtotals <- TRUE
#
# df <- template_data(
#   data = ogd_data_list[["DJS"]][["Amt für Handelsregister und Zivilstandswesen"]][["Ordentliche Einbürgerungen"]][["data"]],
#   header_cols = "jahr",
#   column_cols = c("kennwert_unterteilung", "kennwert"),
#   val_col = "anzahl" ,
#   subtotals = TRUE,
#   subtotals_in_data = FALSE,
#   total_col = TRUE,
#   total_row = TRUE
# )


#' Format Total Rows in a Flextable
#'
#' Applies formatting to designated total rows in a flextable, including bolding or italicizing and drawing horizontal lines.
#'
#' @param ft A `flextable` object.
#' @param row_index Integer vector indicating which rows to format.
#' @param style A character string specifying the formatting style. Options are `"bold"`, `"italic"`, or `"none"`.
#'
#' @return A `flextable` object with formatted total rows.
#' @export
#' @importFrom flextable bold italic hline
format_total_rows <- function(ft, row_index, style = "bold") {
  n_rows <- row_index
  n_rows_top <- subset(n_rows, n_rows > 1) - 1

  if (style == "bold") {
    ft <- ft %>%
      bold(i = n_rows, part = "body") %>%
      hline(i = n_rows, part = "body", border = thin_border()) %>%
      hline(i = n_rows_top, part = "body", border = thin_border())
  } else if (style == "italic") {
    ft <- ft %>%
      italic(i = n_rows, part = "body") %>%
      hline(i = n_rows, part = "body", border = thin_border())
  } else if (style == "none") {
    return(ft)
  }

  return(ft)
}


#' Format Total Column in a Flextable
#'
#' Applies styling to the total column in a flextable, including bold text and a background color.
#'
#' @param ft A `flextable` object.
#' @param col_index Integer index or indices of the column(s) to format.
#'
#' @return A `flextable` object with formatted total column.
#' @export
#' @importFrom flextable bold bg
format_total_col <- function(ft, col_index) {
  ft <- ft %>%
    bold(j = col_index, part = "body") %>%
    bg(j = col_index, part = c("all"), bg = "lightgrey")
  ft
}


#' Format Nested Header in a Flextable
#'
#' Adds a nested header to a flextable using a prepared header mapping and applies consistent formatting.
#'
#' @param ft A `flextable` object.
#' @param template_data A data frame used to extract column keys for header mapping.
#' @param new_names Character vector of names to substitute into the second-level header.
#'
#' @return A `flextable` object with formatted nested header.
#' @export
#' @importFrom flextable set_header_df merge_h merge_v bold valign align hline_bottom
format_nested_header <- function(ft, template_data, new_names) {
  header_df <- prepare_nested_header_df(template_data, new_names)

  ft %>%
    set_header_df(mapping = header_df, key = "col_keys") %>%
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    bold(part = "header") %>%
    valign(valign = "center", part = "header") %>%
    align(align = "center", part = "header", i = 2) %>%
    hline_bottom(part = "header", border = thin_border())
}


#' Prepare Nested Header Data Frame for Flextable
#'
#' Generates a data frame to define a two-level header for use with `flextable::set_header_df()`,
#' by parsing column names and splitting them into hierarchical groups.
#'
#' @param template_data A data frame whose column names contain double underscores (`__`) to define header groups.
#' @param new_names A character string to replace `.label` in the second-level headers.
#'
#' @return A data frame suitable for use as a nested header mapping in `flextable`.
#' @export
#' @importFrom stringr str_split_fixed str_replace
#' @importFrom tools toTitleCase
#' @importFrom dplyr mutate select arrange filter
prepare_nested_header_df <- function(template_data,new_names){
  nested_names_df <- names(template_data) %>%
    stringr::str_split_fixed("__", 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("group", "label")) %>%
    mutate(
      label = ifelse(label == "", NA, label),
      original_name = ifelse(is.na(label), group, paste0(group, "__", label)),
      group = tools::toTitleCase(group),
      label = ifelse(is.na(label), group, label),
      single_level = is.na(label),
      nr = 1:n()
    ) %>%
    arrange(nr)

  col_keys <- nested_names_df$original_name
  template_data <- template_data[, col_keys, drop = FALSE]
  colnames(template_data) <- col_keys


  header_df <- nested_names_df %>%
    mutate(
      level1 = ifelse(single_level, NA, group),
      level2 = label
    ) %>%
    select(col_keys = original_name, level1, level2) %>%
    filter(col_keys != "total_col") %>%
    mutate(level1 = ifelse(level1==level2,NA,level1)) %>%
    mutate(level2 = str_replace(level2,".label",new_names)) %>%
    mutate(level2 = str_replace(level2,".total_col","Total"))


  header_df <- header_df %>%
    group_by(level1) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(level2 = case_when(
      !is.na(level1) & n==1 ~ level1,
      TRUE~ level2
    )) %>%
    mutate(level1 = case_when(
      level1 == level2 ~ NA,
      TRUE~ level1
    )) %>%
    select(-n)


  return(header_df)
}


#' Apply General Formatting to a Flextable
#'
#' Applies consistent styling to numeric and character columns of a flextable, including padding, alignment,
#' font settings, and header styling. It also formats nested headers and sets table properties.
#'
#' @param ft A `flextable` object to be styled.
#' @param fontsize Numeric font size to apply to the whole table (default is 9).
#'
#' @return A styled `flextable` object.
#' @export
#' @importFrom flextable bold padding align colformat_num font fontsize line_spacing hline_top hline_bottom valign set_table_properties autofit
#' @importFrom stringr str_detect
format_general <- function(ft, fontsize=9) {
  num_cols <- which(sapply(ft$body$dataset, is.numeric))
  char_cols <- which(sapply(ft$body$dataset, is.character))


  nested_cols <- which(str_detect(ft$col_keys,"__"))





  ft <- ft %>%
    # border(part = "all",border = thin_border()) %>%
    bold(part = "header") %>%
    padding(j = num_cols, padding.right = 4, part = "all") %>%
    padding(j = char_cols, padding.left = 4, part = "all") %>%
    align(j = num_cols, align = "right", part = "all") %>%
    align(j = char_cols, align = "left", part = "all")  %>%
    colformat_num(big.mark = "'") %>%
    font(fontname = "Arial",part = "all") %>%
    fontsize(size = fontsize,part = "all") %>%
    padding(padding.top = 0, padding.bottom = 0) %>%
    line_spacing(space = 1) %>%
    hline_top(part = "header", border = fat_border()) %>%
    hline_bottom(part = "body", border = fat_border()) %>%
    # hline_bottom(part = "body", border = thin_border()) %>%
    hline_top(part = "body",border = thin_border()) %>%
    valign(valign = "top", part = "all")

  if (length(nested_cols>0)){
    ft <- ft %>%
      align(j = nested_cols,i = 1,part = "header",align = "center")
  }

  ft <- ft %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    autofit()

  return(ft)
}


flextable_turn_header <- function(ft){
  ft %>%
    flextable::rotate(rotation = "btlr",
           part = "header",
           j = 2:ncol(ft$body$dataset)) %>%
    flextable::align(part = "header", align = "right",j = 2:ncol(ft$body$dataset)) %>%
    flextable::valign(valign = "bottom", part = "header",j = 2:ncol(ft$body$dataset)) %>%
    flextable::padding(part = "header",padding.left = 0) %>%
    flextable::set_header_labels(values = setNames("", ft$col_keys[1]))
}



#' Create and Style a Flextable from Template Data
#'
#' Applies formatting, conditional styling, nested headers, and layout adjustments to a flextable
#' generated from structured input data.
#'
#' @param template_data A data frame prepared for flextable formatting (e.g., via `create_template_data()`).
#' @param fontsize Numeric. Font size to use in the table.
#' @param new_names Character vector of new column names to assign.
#' @param inner_border Logical. Whether to apply inner borders to the table.
#'
#' @return A list containing:
#' \describe{
#'   \item{ft}{A formatted `flextable` object.}
#'   \item{data}{The underlying data frame used for rendering.}
#' }
#' @export
#' @importFrom dplyr select any_of rename
#' @importFrom stringr str_detect
#' @importFrom flextable flextable border_inner
create_and_style_flextable <- function(template_data,fontsize,new_names,inner_border,turn_header){
  template_data_ft <- template_data %>%
    dplyr::select(-any_of(c("summary_row", "total_col")))

  if (".total_col" %in% names(template_data)){
    template_data_ft <- template_data_ft %>%
      rename(Total = ".total_col")
  }

  is_nested_header <- any(str_detect(names(template_data_ft), "__"))

  if (!is_nested_header && length(new_names) > 0) {
    names(template_data_ft)[seq_along(new_names)] <- new_names
  }

  ft <- flextable(template_data_ft)


  if ("total_col" %in% names(template_data)){
    if (any(diff(which(template_data$total_col)) == 1) | isTRUE(template_data$total_col[length(template_data$total_col)])){
      style = "none"
    } else {
      style = "bold"
    }
    subtotal_format_rows <- which(template_data$total_col)
    ft <- format_total_rows(ft,subtotal_format_rows,style)
  }
  if ("summary_row" %in% names(template_data)){
    ft <- format_total_rows(ft,nrow(template_data_ft),"bold")

  }
  if (".total_col" %in% names(template_data)){
    ft <- format_total_col(ft,ncol(template_data_ft))
  }
  if (is_nested_header){
    ft <-format_nested_header(ft,template_data_ft,new_names)
  }
  if (inner_border){
    ft <-ft %>%
      border_inner(border = thin_border(),part = "all")
  }




  ft <- format_general(ft)

  if (isTRUE(turn_header)){
    ft <- flextable_turn_header(ft)
  }

  return(list(ft=ft,data = template_data_ft))
}

#' Pre-filter and Clean Input Data for Table Rendering
#'
#' Filters and prepares the input data based on whether it is a timeseries (contains `"jahr"`)
#' or a single-year table. Used as preprocessing before transforming to a flextable structure.
#'
#' @param data A data frame containing at least a `"jahr"` column.
#' @param year Integer. The target year for filtering.
#' @param num_year Integer. Number of years to look back (only for timeseries).
#' @param header_cols Character vector of header column names.
#' @param column_cols Character vector of column group/label names.
#'
#' @return A filtered and cleaned data frame.
#' @export
#' @importFrom dplyr filter arrange select distinct
preprepare_data <- function(data,year,num_year,header_cols,column_cols){
  if ("jahr" %in% c(header_cols,column_cols)){
    # timeseries
    df <- data %>%
      filter(jahr<=as.numeric(year)) %>%
      filter(jahr > (as.numeric(year)-as.numeric(num_year))) %>%
      arrange(jahr) %>%
      distinct()

  } else {
    df <- data %>%
      filter(jahr == year) %>%
      select(-jahr) %>%
      distinct()
  }
  return(df)
}

#' Generate a Styled Flextable from Configuration
#'
#' Processes an element from a nested list structure, prepares the data, and
#' returns a fully styled `flextable` object.
#'
#' @param elem A named list element with configuration metadata and a `data` table.
#' @param year Integer. Year filter for timeseries or single-year data.
#' @param fontsize Numeric. Font size for output table.
#'
#' @return A list with components: \code{ft} (the flextable) and \code{data} (the processed data).
#' @export
#' @importFrom stringr str_split
produce_flextable <- function(elem,year,fontsize =9){

  # Extract configuration options with defaults
  total_row <- isTRUE(as.logical(elem$total_row))
  subtotals <- isTRUE(as.logical(elem$subtotals))
  total_col <- isTRUE(as.logical(elem$total_col))
  subtotals_in_data <- isTRUE(as.logical(elem$subtotals_in_data))
  total_in_data <- isTRUE(as.logical(elem$total_in_data))




  turn_header <- as.logical(elem$turn_header)
  inner_border <- isTRUE(as.logical(elem$inner_border))

  # Parse new names if provided
  new_names <- if (!is.null(elem$new_names)) {
    elem$new_names %>% stringr::str_split(",") %>% unlist()
  } else {
    "Kennwert"
  }

  pre_data <- preprepare_data(data = elem$data,year = year,num_year = elem$num_years,header_cols = elem$header_cols,column_cols = elem$column_cols)

  template_data <-create_template_data(data=pre_data,
                                       header_cols = elem$header_cols,
                                       column_cols = elem$column_cols,
                                       val_col = elem$val_col,
                                       subtotals = subtotals,
                                       subtotals_in_data=subtotals_in_data,
                                       total_in_data = total_in_data,
                                       total_row = total_row,
                                       total_col = total_col)

  if (!is.null(new_names)){
    names(template_data[1:length(new_names)]) <- new_names
  }


  create_and_style_flextable(template_data,fontsize,new_names,inner_border,turn_header)

}




#' Add Metadata to Nested Table List
#'
#' Traverses a 3-level nested list (`department > amt > table`) and adds
#' derived metadata fields to each table list: `column_cols`, `header_cols`, `subtotals`, and `total_in_data`.
#'
#' @param input_list A deeply nested list of lists containing table metadata and data.
#'
#' @return A list of the same structure, enriched with inferred metadata.
#' @export
process_nested_list <- function(input_list) {
  lapply(input_list, function(dept) {
    lapply(dept, function(amt) {
      lapply(amt, function(tbl) {

        # Safely extract values
        colnames_col      <- tbl[["colnames_col"]]
        group_col         <- tbl[["group_col"]]
        label_col         <- tbl[["label_col"]]
        level1_col        <- tbl[["level1_col"]]
        level2_col        <- tbl[["level2_col"]]
        table_type        <- tbl[["table_type"]]
        no_subtotals      <- tbl[["no_subtotals"]]
        total_row_in_data <- tbl[["total_row_in_data"]]

        # column_cols logic
        column_cols <- NULL
        if (!is.null(group_col) && !is.null(label_col)) {
          column_cols <- c(group_col, label_col)
        }

        # header_cols logic
        header_cols <- character(0)
        if (!is.null(colnames_col)) header_cols <- colnames_col
        if (str_detect(table_type,"timeseries")) {
          header_cols <- union(header_cols, "jahr")
        }
        if (!is.null(level1_col) && !is.null(level2_col)) {
          header_cols <- c(level1_col, level2_col)
        }

        # subtotals
        subtotals <- isFALSE(no_subtotals) && !is.null(group_col) && !is.null(label_col)

        # total_in_data
        total_in_data <- isTRUE(total_row_in_data) && !is.null(group_col) && !is.null(label_col)

        # Return updated table with new variables
        c(tbl, list(
          column_cols = column_cols,
          header_cols = header_cols,
          subtotals = subtotals,
          total_in_data = total_in_data
        ))
      })
    })
  })
}

produce_elem <- function(id, table_list, data_list) {
  index <- which(table_list$id == id)

  # Convert row to list and replace NA with NULL
  elem <- lapply(as.list(table_list[index, ]), function(x) if (is.na(x)) NULL else x)

  # Convert specified comma-separated columns to character vectors or NULL
  to_vector_cols <- c("new_names", "column_cols", "header_cols")
  for (col in to_vector_cols) {
    val <- elem[[col]]
    if (!is.null(val) && nzchar(val)) {
      vec <- strsplit(val, ",")[[1]] %>% trimws()
      if (all(vec == "")) {
        elem[[col]] <- NULL
      } else {
        elem[[col]] <- vec
      }
    } else {
      elem[[col]] <- NULL
    }
  }

  # Attach data
  elem$data <- data_list[[id]]

  return(elem)
}

produce_data_list <- function(info_table=table_list, df_list=data_list){


  result <- list()

  for (id in info_table$id){
    elem <-  produce_elem(id = id,info_table,df_list)
    result[[elem$dep]][[elem$amt]][[elem$table]] <- elem
  }

  result
}








create_and_render_quarto <- function(output_dir = "quarto_docs",year,output_word = "test.docx",app_dir = "/r-proj/stat/ogd/stat_anhang/geschaeftsberichtApp/",vorlagen_dir = "/r-proj/stat/ogd/stat_anhang/geschaeftsbericht/vorlagen_urban/" ) {



  source(paste0(app_dir,"R/fct_flextable_full.R"))

  load(paste0(app_dir,"data/data_list.rda"))
  load(paste0(app_dir,"data/table_list.rda"))




  # if (is.null(table_list$raete_summary)){
  #   table_list$raete_summary <- NA
  # }
  # Ensure required packages
  if (!requireNamespace("quarto", quietly = TRUE)) stop("Please install the 'quarto' package")
  if (!requireNamespace("glue", quietly = TRUE)) install.packages("glue")

  library(glue)

  # Header of the Quarto document
  header <- glue::glue(
    "---
title: \"\"
format:
  docx:
    reference-doc: {vorlagen_dir}/vorlage_word2.docx
params:
  year: {year}
  amt: \"Amt für Daten und Statistik\"
execute:
  echo: false
  message: false
  warning: false
---

```{{r setup}}
library(flextable)
library(tidyverse)
library(officer)
library(qrcode)

source(\"{app_dir}R/fct_flextable_full.R\")

# ogd_test <- readRDS(\"../data/final_data_list.rds\")
# ogd_test <- readRDS(\"../data/final_data_list_mod.rds\")
load(\"{app_dir}data/data_list.rda\")
load(\"{app_dir}data/table_list.rda\")
nested_data_ogd <- produce_data_list()

```"
  )

  # Body: Loop through table_list to generate content

  merged_doc <- read_docx(glue::glue("{vorlagen_dir}/vorlage_word2_clean.docx")) %>%
    body_add_par("Anhang I: Statistische Angaben",style = "stat_titel") %>%
    body_add_par("Inhaltsverzeichnis",style = "stat_titel") %>%
    body_add_toc()

  dep_unique <- table_list %>%
    distinct(dep_nr,dep)

  dir.create(output_dir,showWarnings = F)
  for (i in 1:nrow(dep_unique)){

    doc_amt <- dep_unique$dep[i] %>% str_replace_all("ä","ae")
    output_file_dep <- glue::glue("{output_dir}/report_{doc_amt}.qmd")

    print(output_file_dep)
    body_chunks <- list()

    if (dep_unique$dep_nr[i]=="0"){
      body_chunks <- append(body_chunks, glue::glue("# {dep_unique$dep[i]}"))
    } else {
      body_chunks <- append(body_chunks, glue::glue("# {dep_unique$dep_nr[i]}&emsp;{dep_unique$dep[i]}"))
    }

    amt_unique <- table_list %>%
      filter(dep == dep_unique$dep[i]) %>%
      distinct(amt_nr,amt)

    for (k in 1:nrow(amt_unique)){

      if (amt_unique$amt_nr[k]=="pw"){
        body_chunks <- append(body_chunks,glue::glue("## {amt_unique$amt[k]}"))

      } else {
        body_chunks <- append(body_chunks,glue::glue("## {amt_unique$amt_nr[k]}&emsp;{amt_unique$amt[k]}"))

      }

      tables <- table_list %>%
        filter(dep == dep_unique$dep[i]) %>%
        filter(amt == amt_unique$amt[k])

      for (j in 1:nrow(tables)){
        # print(tables$table[j])
        if (!as.logical(tables$data[j])){
          next
        }


        url_encoded <- utils::URLencode(
          glue::glue("https://statistiktg.shinyapps.io/statanhang/?id={tables$id[j]}&year={year}")
        )


        if (isTRUE(as.logical(tables$pagebreak[j]))){

          separator <- "\\newpage"

        } else {
          separator <- "\\"

        }


        ft_code <- glue::glue('```{{r {paste0(tables$dep_nr[j],tables$amt_nr[j],tables$table_nr[j],collapse="_")}, ft.align=\'left\'}}
ft_df <- produce_flextable(elem = nested_data_ogd[["{dep_unique$dep[i]}"]][["{amt_unique$amt[k]}"]][["{tables$table[j]}"]], year = "{year}")
ft_df$ft
```

```{{r {paste0(tables$dep_nr[j],tables$amt_nr[j],tables$table_nr[j],"_qr")},fig.width=42,fig.align=\'right\'}}
qr <- qrcode::qr_code("{url_encoded}")
plot(qr)
```
| [Zur Tabelle]({url_encoded}) |
|:--:|

{separator}

')


        #         ft_code <- glue::glue('```{{r {paste0(tables$dep_nr[j],tables$amt_nr[j],tables$table_nr[j],collapse="_")}, ft.align=\'left\'}}
        # ft_df <- produce_flextable2(elem = ogd_test[["{dep_unique$dep[i]}"]][["{amt_unique$amt[k]}"]][["{tables$table[j]}"]], year = "{year}")
        # ft_df$ft
        # ```
        #
        # ```{{r {paste0(tables$dep_nr[j],tables$amt_nr[j],tables$table_nr[j],"_qr")},fig.width=42,fig.align=\'right\'}}
        # qr <- qrcode::qr_code("{url_encoded}")
        # plot(qr)
        # ```
        # | [Zur Tabelle]({url_encoded}) |
        # |:--:|
        #
        # \\\\
        #
        # ')
        # if (!is.na(tables$landscape[j])){
        #   if (tables$landscape[j]){
        #     body_chunks <- append(body_chunks,glue::glue("::: landscape"))
        #
        #   }
        # }

        if (str_detect(tables$table[j],": Zusammenfassung")){

          tables$table[j] <- str_remove(tables$table[j],": Zusammenfassung") %>% str_trim()
          tables$table[j] <- str_remove(tables$table[j],": Zusammmenfassung") %>% str_trim()

          body_chunks <- append(body_chunks,glue::glue("### {tables$table[j]}"))


        } else {
          body_chunks <- append(body_chunks,glue::glue("### {tables$table[j]}"))


        }

        body_chunks <- append(body_chunks,ft_code)


        # body_chunks <- append(body_chunks,glue::glue("::: {{.keep-together}}"))

        # if (!is.na(tables$landscape[j])){
        #   if (tables$landscape[j]){
        #     body_chunks <- append(body_chunks,glue::glue(":::"))
        #
        #   }
        # }

        # print(tables$pagebreak[j])

      }

    }
    body <- body_chunks %>% paste(collapse="\n\n")


    # Combine and write to .qmd
    full_doc <- paste(c(header, body), collapse = "\n\n")
    writeLines(full_doc, output_file_dep)

    # Render to Word
    quarto::quarto_render(output_file_dep)

    doc_name <- str_replace(output_file_dep,"\\.qmd","\\.docx")


    doc <- read_docx(doc_name) %>%
      headers_replace_all_text(old_value = "DEP",new_value = dep_unique$dep[i]) %>%
      headers_replace_all_text(old_value = "JJJJ", new_value = as.character(year))

    # Save the modified document
    print(doc, target = doc_name)

    merged_doc <- body_add_break(merged_doc, pos = "after")
    merged_doc <- body_end_section_continuous(merged_doc)

    merged_doc <- body_add_docx(merged_doc,doc_name)
  }

  empyt_doc <- officer::read_docx()
  print(empyt_doc, target = "empty.docx")

  merged_doc <- body_add_break(merged_doc, pos = "after")
  merged_doc <- body_end_section_continuous(merged_doc)
  merged_doc <- body_add_docx(merged_doc,"empty.docx")
  unlink("empty.docx")


  print(merged_doc, target = glue::glue("{output_dir}/{output_word}"))



}


# Extract function
# Function to flatten the list
extract_vars <- function(mylist) {
  result <- list()

  for (dep_name in names(mylist)) {
    dep <- mylist[[dep_name]]

    for (amt_name in names(dep)) {
      amt <- dep[[amt_name]]

      for (table_name in names(amt)) {
        table <- amt[[table_name]]
        # take all elements except "data"


        vars <- table

        # if (is.null(vars$data)){
        #   vars$data <- NA
        # } else {
        #   vars$data <- names(vars$data) %>% paste0(collapse = ",")
        # }
        if (is.null(vars$data)){
          vars$data <- FALSE
        } else {
          vars$data <- TRUE
        }
        if (!is.null(vars$new_names)){
          vars$new_names <- vars$new_names %>% paste0(collapse = ",")
        }
        if (!is.null(vars$column_cols)){
          vars$column_cols <- vars$column_cols %>% paste0(collapse = ",")
        }
        if (!is.null(vars$header_cols)){
          vars$header_cols <- vars$header_cols %>% paste0(collapse = ",")
        }
        if (!is.null(vars$ogd_id)){
          vars$ogd_id <- vars$ogd_id %>% paste0(collapse = ",")
        }
        # build row with full hierarchy

        temp <- vars %>% bind_rows() %>%
          mutate_all(as.character)
        result[[length(result) + 1]] <-  temp
      }
    }
  }

  # bind all rows together
  result %>% bind_rows()
}




add_footer_vector <- function(ft,footer_lines){
  ft %>%
    flextable::add_footer_lines(footer_lines) %>%
    font(part = "footer", fontname = "Arial") %>%
    fontsize(part = "footer", size = 9) %>%
    line_spacing(part = "all", space = 0.8) %>%
    padding(padding.top = 3, part = "footer") %>%
    padding(padding.bottom = 0, part = "footer")
}





