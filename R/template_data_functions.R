# Daten mit gruppierten Spalten die von long nach wide konvertiert werden muss  
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


inverse_locf <- function(x) {
  keep <- x != dplyr::lag(x)
  keep[is.na(keep)] <- TRUE
  ifelse(keep, x, NA)
}

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


reverse_template_none <- function(data, original_names = NULL, year_col = "jahr") {
  if (!is.null(original_names)) {
    names(data) <- original_names
  }
  
  data_long <- data %>%
    mutate({{ year_col }} := NA) %>%
    relocate({{ year_col }})
  
  return(data_long)
}


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

