#' Transform Grouped Data from Long to Wide Format with Aggregation
#'
#' Converts a long-format data frame with grouping columns into a wide format.
#' Also aggregates and relabels subgroups under the same group. (for Example Verwaltungsrechtspflege)
#'
#' @param data A data frame in long format.
#' @param group_col Column identifying the group variable.
#' @param label_col Column identifying subgroup labels.
#' @param colnames_col Column containing variable names to pivot wider.
#' @param val_col Column containing values to fill.
#' @param year_col Column indicating the year (default is `"jahr"`).
#'
#' @return A wide-format data frame with aggregated subgroups and labels.
#' @importFrom dplyr rename group_by mutate case_when ungroup summarise_at select bind_rows relocate
#' @importFrom tidyr pivot_wider
#' @export
template_colname_wide_grouped <- function(data,
                                          group_col,
                                          label_col,
                                          colnames_col,
                                          val_col,
                                          year_col = "jahr") {
  df <- data %>%
    rename(
      group = {{ group_col }},
      label = {{ label_col }},
      year = {{ year_col }},
      colnames = {{ colnames_col }},
      value = {{ val_col }}
    )

  temp_df <- df %>%
    pivot_wider(names_from = colnames, values_from = value)

  no_group_data <- temp_df %>%
    group_by(group) %>%
    mutate(.n_group = n(),
           .label = case_when(.n_group == 1 ~ label, # row_number() == 1 ~ as.character(group),
                              TRUE ~ paste0("- ", as.character(label)))) %>%
    ungroup()

  totals_data <- no_group_data %>%
    filter(.n_group > 1)

  grouped_df <- lapply(unique(totals_data$group), function(x) {
    temp_total <- totals_data %>%
      filter(group == x) %>%
      group_by(group) %>%
      summarise_at(vars(all_of(unique(df$colnames))), sum, na.rm = T) %>%
      rename(.label = "group")

    temp <-  totals_data %>%
      filter(group == x) %>%
      select(-c(year, group, .n_group, label))


    temp_total %>%
      bind_rows(temp)


  }) %>% bind_rows()

  result <- no_group_data %>%
    filter(.n_group == 1) %>%
    select(-c(year, group, .n_group, label)) %>%
    relocate(.label) %>%
    bind_rows(grouped_df) %>%
    rename(Kennwert = ".label")

  return(result)
}


#' Simple Template Transformation Without Grouping
#'
#' Removes the year column and optionally renames remaining columns.
#'
#' @param data A data frame.
#' @param new_names Optional vector of new column names.
#' @param year_col Name of the year column to remove (default is `"jahr"`).
#'
#' @return A simplified data frame with renamed columns if specified.
#' @importFrom dplyr rename select
#' @export
template_none <- function(data,
                          new_names = NULL,
                          year_col = "jahr") {

  df <- data %>%
    rename(
      year = {{ year_col }}
    )

  result <- df %>%
    select(-c(year))

  if (!is.null(new_names)){
    names(result) <- new_names
  }

  return(result)
}


#' Transform Data into Nested Header Format
#'
#' Combines two categorical levels (`level1`, `level2`) into a nested column header format for wide transformation.
#'
#' @param data A data frame.
#' @param level1_col Top-level header column.
#' @param level2_col Sub-level header column.
#' @param id_col Identifier column.
#' @param val_col Value column.
#' @param year_col Column indicating the year (default is `"jahr"`).
#'
#' @return A data frame in wide format with nested headers.
#' @importFrom dplyr rename mutate distinct group_by count select
#' @importFrom tidyr pivot_wider
#' @export
template_nested_header <- function(data,level1_col,level2_col,id_col,val_col,year_col = "jahr"){

  df <- data %>%
    rename(
      id = {{ id_col }},
      l1 = {{ level1_col }},
      l2 = {{ level2_col }},
      value = {{ val_col }},
      year = {{ year_col }}

    ) %>%
    mutate(nh = paste0(l1,"__",l2))

  nh_df <- df %>%
    distinct(l1,l2,nh)

  nh_l1 <- nh_df %>%
    group_by(l1) %>%
    count()

  df %>%
    select(-c(year,l1,l2)) %>%
    pivot_wider(names_from = nh,values_from = value)



}


#' Inverse Last Observation Carried Forward (LOCF)
#'
#' Replaces consecutive duplicates with NA, retaining only the first unique value.
#'
#' @param x A vector.
#'
#' @return A vector with consecutive duplicates replaced by NA.
#' @importFrom dplyr lag
#' @export
inverse_locf <- function(x) {
  keep <- x != dplyr::lag(x)
  keep[is.na(keep)] <- TRUE
  ifelse(keep, x, NA)
}



#' Transform to Nested Columns with Level Names
#'
#' Combines hierarchical levels and spreads the data into a wide format. Applies inverse LOCF to higher level.
#'
#' @param data A data frame.
#' @param level1_col Higher-level grouping column.
#' @param level2_col Sub-level column.
#' @param colnames_col Column containing variable names.
#' @param val_col Value column.
#' @param year_col Year column (default is `"jahr"`).
#' @param level1_name Optional name override for `level1_col`.
#' @param level2_name Optional name override for `level2_col`.
#'
#' @return A wide-format data frame with nested columns.
#' @importFrom dplyr rename mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang ensym as_name sym
#' @export
template_nested_col <- function(data, level1_col, level2_col, colnames_col, val_col,
                                year_col = "jahr", level1_name = NULL, level2_name = NULL) {

  # Default name assignments
  if (is.null(level1_name)) level1_name <- rlang::as_name(ensym(level1_col))
  if (is.null(level2_name)) level2_name <- rlang::as_name(ensym(level2_col))

  df <- data %>%
    rename(
      colnames = {{ colnames_col }},
      value = {{ val_col }},
      year = {{ year_col }},
      !!level1_name := {{ level1_col }},
      !!level2_name := {{ level2_col }}
    ) %>%
    pivot_wider(names_from = colnames, values_from = value) %>%
    mutate(!!sym(level1_name) := inverse_locf(!!sym(level1_name))) %>%
    select(-year)

  df
}


#' Reverse Wide Grouped Template to Long Format
#'
#' Converts data from wide back to long format using group and label identifiers.
#'
#' @param data A wide-format data frame.
#' @param group_col Group column name (default is `"group"`).
#' @param label_col Label column name (default is `"label"`).
#' @param year_col Name for the added year column (default is `"jahr"`).
#'
#' @return A long-format data frame.
#' @importFrom tidyr pivot_longer
#' @export
reverse_template_colname_wide_grouped <- function(data, group_col = "group", label_col = "label", year_col = "jahr") {
  data_long <- data %>%
    pivot_longer(
      cols = -c({{ group_col }}, {{ label_col }}),
      names_to = "colnames",
      values_to = "value"
    ) %>%
    rename(
      {{ group_col }} := {{ group_col }},
      {{ label_col }} := {{ label_col }},
      {{ year_col }} := {{ year_col }}
    )

  return(data_long)
}

#' Reverse None Template Transformation
#'
#' Adds a year column back and optionally resets original column names.
#'
#' @param data A simplified wide-format data frame.
#' @param original_names Optional vector of original column names.
#' @param year_col Name for the added year column (default is `"jahr"`).
#'
#' @return A long-format data frame with the year column.
#' @importFrom dplyr mutate relocate
#' @export
reverse_template_none <- function(data, original_names = NULL, year_col = "jahr") {
  if (!is.null(original_names)) {
    names(data) <- original_names
  }

  data_long <- data %>%
    mutate({{ year_col }} := NA) %>%
    relocate({{ year_col }})

  return(data_long)
}


#' Reverse Nested Header Template
#'
#' Splits combined nested headers back into hierarchical columns.
#'
#' @param data A wide-format data frame with nested headers.
#' @param id_col Identifier column (default is `"id"`).
#' @param year_col Year column to add (default is `"jahr"`).
#'
#' @return A long-format data frame with separated levels and values.
#' @importFrom tidyr pivot_longer separate
#' @importFrom dplyr mutate relocate
#' @export
reverse_template_nested_header <- function(data, id_col = "id", year_col = "jahr") {
  data_long <- data %>%
    pivot_longer(
      cols = -{{ id_col }},
      names_to = "nh",
      values_to = "value"
    ) %>%
    separate(nh, into = c("l1", "l2"), sep = "__") %>%
    mutate({{ year_col }} := NA) %>%
    relocate({{ year_col }})

  return(data_long)
}

#' Reverse Nested Column Template Transformation
#'
#' Transforms nested wide data back to long format while restoring hierarchical columns.
#'
#' @param data A data frame with nested column structure.
#' @param level1_col Higher-level grouping column.
#' @param level2_col Sub-level column.
#' @param colnames_col Name for the output column storing variable names (default is `"colnames"`).
#' @param val_col Name for the output column storing values (default is `"value"`).
#' @param year_col Name for the added year column (default is `"jahr"`).
#'
#' @return A long-format data frame with group-level hierarchy.
#' @importFrom dplyr mutate relocate
#' @importFrom tidyr pivot_longer
#' @importFrom zoo na.locf
#' @export
reverse_template_nested_col <- function(data, level1_col, level2_col, colnames_col = "colnames", val_col = "value", year_col = "jahr") {
  level1_name <- rlang::as_name(ensym(level1_col))
  level2_name <- rlang::as_name(ensym(level2_col))

  data_long <- data %>%
    mutate({{ level1_col }}:=zoo::na.locf({{ level1_col }})) %>%
    pivot_longer(
      cols = -c({{ level1_col }}, {{ level2_col }}),
      names_to = {{ colnames_col }},
      values_to = {{ val_col }}
    ) %>%
    mutate({{ year_col }} := NA) %>%
    relocate({{ year_col }})

  return(data_long)
}

