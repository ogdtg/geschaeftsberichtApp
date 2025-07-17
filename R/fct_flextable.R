#' Flextable Creation and Formatting Functions
#'
#' A comprehensive set of functions for creating and formatting flextables
#' with various layouts, nested headers, and styling options.

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Convert All Numeric Columns If Possible
#'
#' Attempts to convert all columns in a data frame to numeric where possible,
#' while preserving the original data type if conversion would introduce NAs.
#' Special handling for 'jahr' column to keep as character.
#'
#' @param df A data frame to process
#' @return Data frame with numeric columns converted where appropriate
#' @export
convert_all_numeric_if_possible <- function(df) {
  df %>%
    mutate(across(everything(), ~ {
      col <- .x
      converted <- suppressWarnings(as.numeric(as.character(col)))
      if (all(is.na(col) == is.na(converted))) converted else col
    })) %>%
    mutate(jahr = as.character(jahr))
}

#' Create Thin Border Style
#'
#' Creates a thin black border for flextable formatting.
#'
#' @return An officer border object with thin black line
#' @export
thin_border <- function() {
  officer::fp_border(color = "black", width = .5)
}

#' Create Fat Border Style
#'
#' Creates a thick black border for flextable formatting.
#'
#' @return An officer border object with thick black line
#' @export
fat_border <- function() {
  officer::fp_border(color = "black", width = 1)
}


prepare_nested_header_df <- function(template_data){
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

  return(list(nested_names_df=nested_names_df,template_data = template_data))
}

# =============================================================================
# CORE FLEXTABLE CREATION FUNCTIONS
# =============================================================================

#' Add Total Calculations to Data
#'
#' Helper function to add row and/or column totals to template data.
#'
#' @param template_data Data frame to add totals to
#' @param total_row Logical, whether to add a total row
#' @param total_col Logical, whether to add a total column
#' @param filter_condition Optional filter condition for total row calculation
#' @return Modified data frame with totals added
add_totals <- function(template_data, total_row = FALSE, total_col = FALSE, filter_condition = NULL,total_row_in_data = NULL) {

  # Add total column (row-wise sums)
  if (total_col) {
    template_data$Total <- template_data %>%
      select(where(is.numeric)) %>%
      rowSums(na.rm = TRUE)
  }

  # Add total row (column-wise sums)
  if (total_row) {

    id_name <- names(template_data)[1]

    if (is.null(total_row_in_data)){
      if (!is.null(filter_condition)) {
        summary_data <- template_data %>% filter(!!sym(filter_condition)) %>% mutate_at(vars(all_of(filter_condition)),as.logical)
      } else {
        summary_data <- template_data %>% filter(!str_detect(.[[1]], "^- "))
      }

      summary_row <- summary_data %>%
        purrr::map(~ if (is.numeric(.)) sum(., na.rm = TRUE) else "") %>%
        as.data.frame() %>%
        setNames(names(template_data))

      if (!is.null(filter_condition)) {
        summary_row <- summary_row %>%
          mutate_at(vars(all_of(filter_condition)),as.logical)
      }

      summary_row <- summary_row %>%
        mutate(!!sym(id_name) := "Total")
    } else {


      summary_row <- template_data %>%
        filter(!!sym(id_name)==total_row_in_data)


      template_data <- template_data %>%
        filter(!!sym(id_name)!=total_row_in_data)


    }


    # Handle special columns
    if ("total_col" %in% names(summary_row)) {
      summary_row$total_col <- as.logical(summary_row$total_col)
    }

    template_data <- bind_rows(template_data, summary_row)
  }

  return(template_data)
}

#' Apply Standard Flextable Formatting
#'
#' Applies consistent formatting to flextables including alignment, fonts, and borders.
#'
#' @param ft A flextable object
#' @param template_data The original data used to create the flextable
#' @param total_row Logical, whether total row styling should be applied
#' @param total_col Logical, whether total column styling should be applied
#' @param fontsize Font size for the table (default: 9)
#' @return Formatted flextable object
apply_standard_formatting <- function(ft, template_data, total_row = FALSE, total_col = FALSE, fontsize = 9,bold_subtotals = TRUE) {

  # Format numeric columns
  num_cols <- names(template_data)[purrr::map_lgl(template_data, is.numeric)]
  if (length(num_cols) > 0) {
    ft <- colformat_num(ft, j = num_cols, big.mark = "'", digits = 0)
  }

  # Align character and numeric columns
  char_cols <- names(template_data)[purrr::map_lgl(template_data, is.character)]
  if (length(char_cols) > 0) {
    ft <- align(ft, j = char_cols, align = "left", part = "all")
  }

  other_cols <- setdiff(names(template_data), char_cols)
  ft <- align(ft, j = other_cols, align = "center", part = "all")

  # Style total column
  if (total_col) {
    n_cols <- ncol(template_data)
    ft <- ft %>%
      bold(j = n_cols, part = "body") %>%
      bg(j = n_cols, part = "all", bg = "lightgrey")
  }

  # Style total row
  if (total_row) {
    n_rows <- nrow(template_data)
    ft <- ft %>%
      bold(i = n_rows, part = "body") %>%
      hline(i = n_rows - 1, part = "body", border = thin_border())
  }



  # Apply final formatting
  ft <- ft %>%
    bold(part = "header") %>%
    valign(valign = "center", part = "header") %>%
    hline_bottom(part = "header", border = thin_border()) %>%
    padding(padding.top = 0, padding.bottom = 0, padding.left = 0, padding.right = 0) %>%
    line_spacing(space = 1) %>%
    fontsize(size = fontsize, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    hline_top(part = "header", border = fat_border()) %>%
    hline_bottom(part = "body", border = fat_border()) %>%

    autofit()

  return(ft)
}

#' Create Flextable with Nested Headers
#'
#' Generates a flextable object from a data frame where column names represent
#' nested headers (e.g., "Category__Subcategory").
#'
#' @param template_data A data frame with column names using double underscores to indicate nesting
#' @param total_row Logical, whether to append a row with column totals
#' @param total_col Logical, whether to append a column with row totals
#' @return A formatted flextable object with nested headers and optional totals
#' @importFrom dplyr mutate bind_rows select where n
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_split_fixed
#' @importFrom flextable flextable set_header_df merge_h merge_v
#' @export
create_flextable_nested_header <- function(template_data, total_row = FALSE, total_col = FALSE) {

  # Parse nested column names
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

  # Add totals using helper function
  if (total_col) {
    template_data$row_total <- template_data %>%
      select(where(is.numeric)) %>%
      rowSums(na.rm = TRUE)

    nested_names_df <- bind_rows(
      nested_names_df,
      data.frame(
        group = "Total", label = "Total", original_name = "row_total",
        single_level = TRUE, nr = max(nested_names_df$nr) + 1,
        stringsAsFactors = FALSE
      )
    )
  }

  template_data <- add_totals(template_data, total_row = total_row, total_col = FALSE)

  # Create header structure
  header_df <- nested_names_df %>%
    mutate(
      level1 = ifelse(single_level, NA, group),
      level2 = label
    ) %>%
    select(col_keys = original_name, level1, level2)

  # Create and format flextable
  ft <- flextable(template_data, col_keys = header_df$col_keys) %>%
    set_header_df(mapping = header_df, key = "col_keys") %>%
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    bold(part = "header", i = 1) %>%
    valign(valign = "center", part = "header") %>%
    align(align = "center", part = "header", i = 1) %>%
    vline(j = ncol(template_data))

  # Apply standard formatting with special handling for nested headers
  ft <- apply_standard_formatting(ft, template_data, total_row, total_col)

  return(ft)
}

#' Create Simple Flextable
#'
#' Builds a basic flextable object from a wide-format data frame, with optional row and column totals.
#'
#' @param template_data A data frame to be formatted
#' @param total_row Logical, whether to add a summary row
#' @param total_col Logical, whether to add a summary column
#' @param subtotals Logical, whether to add subtotal formatting
#' @return A formatted flextable object
#' @export
create_flextable_simple <- function(template_data, total_row = FALSE, total_col = FALSE, subtotals = FALSE) {

  # Handle subtotals preparation
  if (subtotals) {
    n_rows <- which(template_data$total_col)
    n_rows_top <- subset(n_rows, n_rows > 1) - 1
    template_data$total_col <- NULL
  }

  # Add totals
  template_data <- add_totals(template_data, total_row, total_col)

  # Create basic flextable
  ft <- flextable(template_data)

  # Apply subtotal formatting if needed
  if (subtotals && exists("n_rows")) {
    ft <- ft %>%
      bold(i = n_rows, part = "body") %>%
      hline(i = n_rows, part = "body", border = thin_border()) %>%
      hline(i = n_rows_top, part = "body", border = thin_border())
  }

  # Apply standard formatting
  ft <- apply_standard_formatting(ft, template_data, total_row, total_col) %>%
    align(align = "left", part = "all")

  return(ft)
}


format_subtotals <- function(ft,total_col_index,style = "bold"){



  n_rows <- total_col_index
  n_rows_top <- subset(n_rows, n_rows > 1) - 1


  if (style == "bold"){

    ft <- ft %>%
      bold(i = n_rows, part = "body") %>%
      hline(i = n_rows, part = "body", border = thin_border()) %>%
      hline(i = n_rows_top, part = "body", border = thin_border())
  } else if (style == "italic"){

    ft <- ft %>%
      italic(i = n_rows, part = "body") %>%
      hline(i = n_rows, part = "body", border = thin_border())

  } else if (style == "none"){
    return(ft)
  }

  return(ft)



}


#' Create Flextable for GRGEKO Format
#'
#' Creates a specialized flextable format for GRGEKO reports.
#'
#' @param template_data A data frame to be formatted
#' @return A formatted flextable object
#' @export
create_flextable_grgeko <- function(template_data) {
  flextable(template_data) %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    style(pr_c = fp_cell(vertical.align = "top")) %>%
    hline(part = "header", border = fp_border(width = 0.5)) %>%
    hline_top(border = fp_border(width = 0.5), part = "header") %>%
    hline_bottom(border = fp_border(width = 0.5), part = "body") %>%
    font(part = "all", fontname = "Arial") %>%
    fontsize(size = 9, part = "all")
}

#' Create Flextable for Subsections
#'
#' Creates a flextable with subsection formatting and totals handling.
#'
#' @param template_data A data frame to be formatted
#' @param total_row Logical, whether to add a summary row
#' @param total_col Logical, whether to add a summary column
#' @param new_names Optional vector of new column names
#' @return A formatted flextable object
#' @export
create_flextable_subsection <- function(template_data, total_row = FALSE, total_col = FALSE, new_names = NULL) {

  # Add totals with special filter condition
  template_data <- add_totals(template_data, total_row = FALSE, total_col = total_col)

  if (total_row) {
    template_data <- add_totals(template_data, total_row = TRUE, total_col = FALSE, filter_condition = "total")
    template_data$total <- c(rep(FALSE, nrow(template_data) - 1), TRUE)
  }

  # Identify total rows for formatting
  n_rows <- which(template_data$total)
  n_rows_top <- subset(n_rows, n_rows > 1) - 1
  template_data$total <- NULL

  # Create flextable
  ft <- flextable(template_data) %>%
    bold(i = n_rows, part = "body") %>%
    hline(i = n_rows, part = "body", border = thin_border()) %>%
    hline(i = n_rows_top, part = "body", border = thin_border()) %>%
    border(i = n_rows, part = "body",
           border.right = officer::fp_border(width = 0),
           border.left = officer::fp_border(width = 0))

  # Apply new names if provided
  if (!is.null(new_names)) {
    ft <- set_header_labels(ft, values = setNames(new_names, names(template_data)))
  }

  # Apply standard formatting
  ft <- apply_standard_formatting(ft, template_data, total_row, total_col) %>%
    align(align = "left", part = "all")

  return(ft)
}

#' Create GRGEKO Summary Flextable
#'
#' Creates a specialized summary table for GRGEKO reports with year-specific status entries.
#'
#' @param template_data A data frame containing Status and Anzahl columns
#' @param year The reporting year for status labels
#' @return A formatted flextable object without headers
#' @export
create_flextable_grgeko_summary <- function(template_data, year) {

  berichtsjahr <- as.numeric(year)
  vorjahr <- berichtsjahr - 1
  folgejahr <- berichtsjahr + 1

  # Define desired order of status entries based on year
  status_order <- c(
    paste0("pendent am 01.01.", berichtsjahr, ":"),
    "neu eingegangen im Berichtsjahr: ",
    "erledigt im Berichtsjahr:",
    paste0("pendent am 01.01.", folgejahr, ":")
  )

  # Prepare data: remove year, order status
  processed_data <- template_data %>%
    select(Status, Anzahl) %>%
    mutate(Status = factor(Status, levels = status_order)) %>%
    arrange(Status) %>%
    mutate(Status = as.character(Status))

  # Create flextable
  ft <- flextable(processed_data) %>%
    delete_part(part = "header") %>%
    align(j = 1, align = "left", part = "all") %>%
    align(j = 2, align = "right", part = "all") %>%
    border_remove()

  return(ft)
}


create_flextable_nested_col_nested_header <- function(template_data, total_row = FALSE, total_col = FALSE, subtotals = FALSE, total_row_in_data = NULL) {

  # Parse nested column names
  df <- prepare_nested_header_df(template_data )



  # Add totals using helper function
  if (total_col) {
    df$template_data$row_total <- df$template_data %>%
      select(where(is.numeric)) %>%
      rowSums(na.rm = TRUE)

    df$nested_names_df <- bind_rows(
      df$nested_names_df,
      data.frame(
        group = "Total", label = "Total", original_name = "row_total",
        single_level = TRUE, nr = max(df$nested_names_df$nr) + 1,
        stringsAsFactors = FALSE
      )
    )
  }


  df$template_data <- add_totals(df$template_data, total_row = total_row, total_col = FALSE,total_row_in_data = "Total")


  if ("total_col" %in% names(df$template_data)){
    total_col_index <- which( df$template_data[["total_col"]])
    df$template_data$total_col <- NULL
  }

  # Create header structure
  header_df <- df$nested_names_df %>%
    mutate(
      level1 = ifelse(single_level, NA, group),
      level2 = label
    ) %>%
    select(col_keys = original_name, level1, level2) %>%
    filter(col_keys != "total_col") %>%
    mutate(level1 = ifelse(level1==level2,NA,level1))

  # Create and format flextable
  ft <- flextable(df$template_data, col_keys = header_df$col_keys) %>%
    set_header_df(mapping = header_df, key = "col_keys") %>%
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    bold(part = "header") %>%
    valign(valign = "center", part = "header") %>%
    align(align = "center", part = "header", i = 2) %>%
    vline(j = ncol(df$template_data)) %>%
    border_remove()


  # Apply standard formatting with special handling for nested headers
  ft <- apply_standard_formatting(ft, df$template_data, total_row, total_col)


  ft <- format_subtotals(ft,total_col_index)

  return(ft)

}

#' Identify Unnested Columns in Flextable Header
#'
#' Extracts indices of columns in a flextable object that do not contain hierarchical headers.
#'
#' @param ft A flextable object with a nested header structure
#' @return A numeric vector of column indices that are unnested
#' @export
get_unnested_cols <- function(ft) {
  ft$header$dataset %>%
    t() %>%
    data.frame() %>%
    mutate(same = X1 == X2) %>%
    pull(same) %>%
    which()
}

# =============================================================================
# MAIN PRODUCTION FUNCTION
# =============================================================================

#' Generate Flextable and Return Template Data
#'
#' Main function that creates flextables based on configuration elements.
#' Supports multiple table types and formatting options.
#'
#' @param elem Optional. A single table element configuration
#' @param data_list A nested list of table data and metadata
#' @param dep Department identifier in the data list
#' @param amt Amount identifier in the data list
#' @param table Table identifier
#' @param year The year to filter the table data
#' @param fontsize Font size for the table (default: 9)
#' @return A list with 'ft' (flextable object) and 'data' (processed data)
#' @export
produce_flextable2 <- function(elem = NULL, data_list = NULL, dep = NULL, amt = NULL,
                               table = NULL, year, fontsize = 9) {

  # Get element from data_list if not provided
  if (is.null(elem)) {
    elem <- data_list[[dep]][[amt]][[table]]
  }

  # Extract configuration options with defaults
  total_row <- !is.null(elem$total_row)
  no_subtotals <- !is.null(elem$no_subtotals)
  total_col <- !is.null(elem$total_col)
  turn_header <- !is.null(elem$turn_header)
  inner_border <- !is.null(elem$inner_border)

  # Parse new names if provided
  new_names <- if (!is.null(elem$new_names)) {
    elem$new_names %>% stringr::str_split(",") %>% unlist()
  } else {
    NULL
  }

  # Route to appropriate table creation function based on type
  result <- switch(elem$table_type,
                   "nested_header" = create_nested_header_table(elem, year, total_row, total_col),
                   "wide" = create_wide_table(elem, year, total_row, total_col, new_names),
                   "none" = create_none_table(elem, year, total_row, total_col, new_names),
                   "timeseries" = create_timeseries_table(elem, total_row, total_col, new_names),
                   "timeseries_nested_col" = create_timeseries_nested_table(elem, total_row, total_col, no_subtotals, new_names),
                   "nested_col_subsection" = create_nested_subsection_table(elem, year, total_row, total_col, new_names),
                   "nested_col" = create_nested_col_table(elem, year, total_row, total_col, new_names),
                   "colname_wide_grouped" = create_colname_wide_grouped_table(elem, year, total_row, total_col),
                   "grgeko" = create_grgeko_table(elem, year, new_names),
                   "nested_col_nested_header" = create_nested_col_nested_header_table(elem, year, total_row, total_col),

                   stop("Unknown table_type: ", elem$table_type)
  )

  ft <- result$ft
  template_data <- result$data

  # Apply special formatting options
  if (turn_header) {
    ft <- ft %>%
      rotate(rotation = "btlr", part = "header") %>%
      align(align = "left", part = "header") %>%
      height(height = 2, part = "header") %>%
      hrule(i = 1, rule = "exact", part = "header")
  }

  if (inner_border) {
    ft <- ft %>%
      border_inner(part = "body", border = thin_border())
  }

  # Apply final alignment and formatting
  ft <- apply_final_alignment(ft, elem$table_type, template_data, fontsize)

  return(list(ft = ft, data = template_data))
}

# =============================================================================
# HELPER FUNCTIONS FOR SPECIFIC TABLE TYPES
# =============================================================================

create_nested_col_nested_header_table <- function(elem, year, total_row, total_col) {

  if (isTRUE(elem$colnames_col == "jahr")){
    temp_data <- elem$data
  } else {
    temp_data <- elem$data %>% dplyr::filter(jahr == year)
  }



  template_data <- template_nested_col_nested_header(
    data = temp_data,
    group_col = elem$group_col,
    label_col = elem$label_col,
    level1_col = elem$level1_col,
    level2_col = elem$level2_col,
    val_col = elem$val_col,
    no_subtotals = elem$no_subtotals,
    new_names = elem$new_names,
    subtotals_in_data = elem$subtotals_in_data
  )

  ft <- create_flextable_nested_col_nested_header(template_data, total_row = total_row, total_col = total_col, subtotals = !elem$no_subotals, total_row_in_data = elem$total_row_in_data)

  list(ft = ft, data = template_data)
}


create_nested_header_table <- function(elem, year, total_row, total_col) {
  temp_data <- elem$data %>% dplyr::filter(jahr == year)
  template_data <- template_nested_header(
    data = temp_data,
    level1_col = elem$level1_col,
    level2_col = elem$level2_col,
    id_col = elem$id_col,
    val_col = elem$val_col,
    year_col = "jahr"
  )
  ft <- create_flextable_nested_header(template_data, total_row, total_col)
  list(ft = ft, data = template_data)
}


create_wide_table <- function(elem, year, total_row, total_col, new_names) {
  temp_data <- elem$data %>% dplyr::filter(jahr == year)
  template_data <- template_wide(
    temp_data,
    year_col = "jahr",
    val_col = elem$val_col,
    colnames_col = elem$colnames_col,
    new_names = new_names
  )
  ft <- create_flextable_simple(template_data, total_row, total_col)
  list(ft = ft, data = template_data)
}

create_none_table <- function(elem, year, total_row, total_col, new_names) {
  temp_data <- elem$data %>% dplyr::filter(jahr == year)
  template_data <- template_none(
    data = temp_data,
    new_names = new_names,
    year_col = "jahr"
  )
  ft <- create_flextable_simple(template_data, total_row, total_col)
  list(ft = ft, data = template_data)
}

create_timeseries_table <- function(elem, total_row, total_col, new_names) {
  template_data <- template_timeseries(
    data = elem$data,
    val_col = elem$val_col,
    year_col = "jahr",
    num_years = elem$num_years,
    new_names = new_names
  )
  ft <- create_flextable_simple(template_data, total_row, total_col)
  list(ft = ft, data = template_data)
}

create_timeseries_nested_table <- function(elem, total_row, total_col, no_subtotals, new_names) {
  temp_data <- elem$data
  template_data <- template_timeseries_nested_col(
    data = temp_data,
    group_col = elem$group_col,
    label_col = elem$label_col,
    year_col = "jahr",
    val_col = elem$val_col,
    num_years = elem$num_years,
    no_subtotals = no_subtotals,
    new_names = new_names
  )
  ft <- create_flextable_simple(template_data, total_row, total_col, subtotals = TRUE)
  list(ft = ft, data = template_data)
}

create_nested_subsection_table <- function(elem, year, total_row, total_col, new_names) {
  temp_data <- elem$data %>% dplyr::filter(jahr == year)
  template_data <- template_nested_col_subsection(
    data = temp_data,
    level1_col = elem$level1_col,
    level2_col = elem$level2_col,
    val_col = elem$val_col,
    colnames_col = elem$colnames_col,
    year_col = "jahr",
    level1_name = elem$level1_name,
    level2_name = elem$level2_name
  )
  ft <- create_flextable_subsection(template_data, total_row, total_col, new_names = new_names)
  list(ft = ft, data = template_data)
}

create_nested_col_table <- function(elem, year, total_row, total_col, new_names) {
  temp_data <- elem$data %>% dplyr::filter(jahr == year)
  template_data <- template_nested_col(
    data = temp_data,
    group_col = elem$group_col,
    label_col = elem$label_col,
    val_col = elem$val_col,
    colnames_col = elem$colnames_col,
    no_subtotals = elem$no_subtotals,
    new_names = new_names
  )
  ft <- create_flextable_simple(template_data, total_row, total_col, subtotals = TRUE)
  list(ft = ft, data = template_data)
}

create_colname_wide_grouped_table <- function(elem, year, total_row, total_col) {
  temp_data <- elem$data %>% dplyr::filter(jahr == year)
  template_data <- template_colname_wide_grouped(
    data = temp_data,
    group_col = elem$group_col,
    label_col = elem$label_col,
    colnames_col = elem$colnames_col,
    val_col = elem$val_col,
    year_col = "jahr"
  )
  ft <- create_flextable_simple(template_data, total_row, total_col)
  list(ft = ft, data = template_data)
}

create_grgeko_table <- function(elem, year, new_names) {
  temp_data <- elem$data %>% dplyr::filter(jahr == year)
  template_data <- template_none(data = temp_data, new_names = new_names, year_col = "jahr")

  if (isTRUE(elem$raete_summary)) {
    ft <- create_flextable_grgeko_summary(template_data, year)
  } else {
    ft <- create_flextable_grgeko(template_data)
  }

  list(ft = ft, data = template_data)
}

apply_final_alignment <- function(ft, table_type, template_data, fontsize) {
  num_cols <- which(sapply(ft$body$dataset, is.numeric))
  char_cols <- which(sapply(ft$body$dataset, is.character))

  if (table_type == "nested_header") {
    unnested_cols <- get_unnested_cols(ft)
    num_cols_unn <- num_cols[num_cols %in% unnested_cols]
    char_cols_unn <- char_cols[char_cols %in% unnested_cols]

    ft <- ft %>%
      align(j = num_cols, align = "center", part = "header", i = 1) %>%
      align(j = num_cols, align = "right", part = "header", i = 2) %>%
      align(j = num_cols, align = "right", part = "body") %>%
      align(j = char_cols, align = "left", part = "header", i = 2) %>%
      align(j = char_cols, align = "left", part = "body")

    if (length(num_cols_unn) > 0) {
      ft <- ft %>% align(align = "right", part = "header", j = num_cols_unn)
    }

    if (length(char_cols_unn) > 0) {
      ft <- ft %>% align(align = "left", part = "header", j = char_cols_unn)
    }
  } else {
    ft <- ft %>%
      align(j = num_cols, align = "right", part = "all") %>%
      align(j = char_cols, align = "left", part = "all")
  }

  ft %>%
    padding(j = num_cols, padding.right = 4, part = "all") %>%
    padding(j = char_cols, padding.left = 4, part = "all") %>%
    font(fontname = "Arial",part = "all") %>%
    fontsize(size = fontsize,part = "all") %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    autofit()
}
