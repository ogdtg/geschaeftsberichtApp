
# Flextable Functions -----------------------------------------------------

library(flextable)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

create_flextable_nested_header <- function(template_data, total_row = FALSE, total_col = FALSE) {
  
  nested_names_df <- names(template_data) %>%
    str_split_fixed("__", 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("group", "label")) %>%
    mutate(label = ifelse(label == "", NA, label),
           original_name = ifelse(is.na(label), group, paste0(group, "__", label)),
           group = tools::toTitleCase(group),
           label = ifelse(is.na(label), group, label),
           single_level = is.na(label)) %>%
    mutate(nr = 1:n()) %>%
    arrange(nr)
  
  col_keys <- nested_names_df$original_name
  template_data <- template_data[, col_keys, drop = FALSE]
  colnames(template_data) <- col_keys
  
  # Add total column if requested
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
  
  # Add total row if requested
  if (total_row) {
    summary_row <- map(template_data, function(col) {
      if (is.numeric(col)) sum(col, na.rm = TRUE)
      else ""
    }) %>% as.data.frame()
    
    id_name <- names(template_data)[1]
    
    summary_row <- summary_row %>%
      setNames(names(template_data)) %>%
      mutate(!!sym(id_name) := "Total")
    
    
    template_data <- bind_rows(template_data, summary_row)
  }
  
  header_df <- nested_names_df %>%
    mutate(level1 = ifelse(single_level, NA, group),
           level2 = label) %>%
    select(col_keys = original_name, level1, level2)
  
  ft <- flextable(template_data, col_keys = header_df$col_keys) %>%
    set_header_df(mapping = header_df, key = "col_keys") %>%
    merge_h(part = "header") %>%
    merge_v(part = "header") %>%
    theme_booktabs() %>%
    border_inner_v() %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    bold(part = "header",i=1) %>%
    valign(valign = "center", part = "header") %>% 
    align(align = "center", part = "header",i=1) %>% 
    
    vline(j = ncol(template_data)) 
  
  num_cols <- names(template_data)[map_lgl(template_data, is.numeric)]
  if (length(num_cols) > 0) {
    ft <- colformat_num(ft, j = num_cols, big.mark = "'", digits = 0)
  }
  
  char_cols <- names(template_data)[map_lgl(template_data, is.character)]
  if (length(char_cols) > 0) {
    ft <- align(ft, j = char_cols, align = "left", part = "all")
  }
  
  other_cols <- setdiff(names(template_data), char_cols)
  ft <- align(ft, j = other_cols, align = "center", part = "all")
  
  
  if (total_col){
    n_cols <- ncol(template_data)
    ft <- ft %>%
      bold(j = n_cols, part = "body") %>% # Bold last column
      bg(j = n_cols, part = "all",bg = "lightgrey")
  }
  
  if (total_row){
    n_rows <- nrow(template_data)
    ft <- ft %>%
      bold(i = n_rows, part = "body")  %>%     # Bold last row
      hline(i = n_rows - 1, part = "body")
  }
  
  ft %>% 
    autofit() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    padding(padding.top = 0, padding.bottom = 0, padding.left = 0, padding.right = 0) %>%
    line_spacing(space = 1)
  

}




create_flextable_simple <- function(template_data, total_row = FALSE, total_col = FALSE) {
  
  # Total column (row-wise sums)
  if (total_col) {
    template_data$Total <- template_data %>%
      select(where(is.numeric)) %>%
      rowSums(na.rm = TRUE)
  }
  
  # Total row (column-wise sums)
  if (total_row) {
    summary_row <- map(template_data, ~ if (is.numeric(.)) sum(., na.rm = TRUE) else "") %>%
      as.data.frame() %>%
      setNames(names(template_data))
    
    id_name <- names(template_data)[1]
    summary_row <- summary_row %>%
      mutate(!!sym(id_name) := "Total")
    
    template_data <- bind_rows(template_data, summary_row)
  }
  
  ft <- flextable(template_data) %>%
    theme_booktabs() %>%
    border_inner_v() %>%
    fontsize(size = 9, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    bold(part = "header") %>%
    valign(valign = "center", part = "header")
  
  # Format numeric columns
  num_cols <- names(template_data)[map_lgl(template_data, is.numeric)]
  if (length(num_cols) > 0) {
    ft <- colformat_num(ft, j = num_cols, big.mark = "'", digits = 0)
  }
  
  # Align character and other columns
  char_cols <- names(template_data)[map_lgl(template_data, is.character)]
  if (length(char_cols) > 0) {
    ft <- align(ft, j = char_cols, align = "left", part = "all")
  }
  other_cols <- setdiff(names(template_data), char_cols)
  ft <- align(ft, j = other_cols, align = "center", part = "all")
  
  # Style total col
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
      hline(i = n_rows - 1, part = "body")
  }
  
  ft <- hline_top(ft, part = "body")

  ft %>% 
    autofit() %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    padding(padding.top = 0, padding.bottom = 0, padding.left = 0, padding.right = 0) %>%
    line_spacing(space = 1) %>%
    align(align = "left", part = "all")
  

}

get_unnested_cols <- function(ft){
  ft$header$dataset %>% 
    t() %>% 
    data.frame() %>% 
    mutate(same = X1==X2) %>% 
    pull(same) %>% 
    which()
  
}

produce_flextable <- function(data_list,dep,amt,table,year){
  
  elem <- data_list[[dep]][[amt]][[table]]
  
  total_row <- ifelse(is.null(elem$total_row),FALSE,TRUE)
  total_col <- ifelse(is.null(elem$total_col),FALSE,TRUE)
  turn_header <- ifelse(is.null(elem$turn_header),FALSE,TRUE)
  inner_border <- ifelse(is.null(elem$inner_border),FALSE,TRUE)
  
  if (elem$table_type=="nested_header"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year)

    template_data <- template_nested_header(data = temp_data,level1_col = elem$level1_col,level2_col = elem$level2_col,id_col = elem$id_col,val_col = elem$val_col,year_col = "jahr")
    
    ft <- create_flextable_nested_header(template_data,total_row,total_col)
  }
  
  if (elem$table_type=="none"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year) 
    
    if (!is.null(elem$new_names)){
      new_names <- elem$new_names %>% str_split(",") %>% unlist()
    }
    
    template_data <- template_none(data = temp_data,new_names = new_names,year_col = "jahr")
    
    ft <- create_flextable_simple(template_data,total_row,total_col)
    
  }
  
  if (elem$table_type=="nested_col"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year)
    
    if (!is.null(elem$new_names)){
      new_names <- elem$new_names %>% str_split(",") %>% unlist()
    }
    
    template_data <- template_nested_col(data = temp_data,level1_col = elem$level1_col,level2_col = elem$level2_col,colnames_col = elem$colnames_col,val_col = elem$val_col,year_col = "jahr",level1_name = elem$level1_name,level2_name = elem$level2_name)
    
    ft <- create_flextable_simple(template_data,total_row,total_col)
    
  }
  
  
  if (elem$table_type=="colname_wide_grouped"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year)
    
    
    
    template_data <- template_colname_wide_grouped(data = temp_data,group_col = elem$group_col,label_col = elem$label_col,colnames_col = elem$colnames_col,val_col = elem$val_col,year_col = "jahr")
    
    ft <- create_flextable_simple(template_data,total_row,total_col)
    

    
    
  }
  
  if (turn_header) {
    
    add_invisible_line <- function(x) paste0(x, "\n\n")
    
    ft <- ft %>%
      rotate(rotation = "btlr", part = "header") %>%
      # align(align = "left", part = "body") %>% 
      # set_header_labels(Var1 = " " ) %>% 
      align(align = "left", part = "header") %>%
      height(height = 2, part = "header") %>%
      hrule(i = 1, rule = "exact", part = "header")
  }
  
  if (inner_border){
    ft <- ft %>% 
      flextable::border_inner(part = "body")
  }
  
  
  num_cols <- which(sapply( ft$body$dataset, is.numeric))
  char_cols <- which(sapply( ft$body$dataset, is.character))
  
  
  
  if (elem$table_type=="nested_header"){
    
    unnested_cols <- get_unnested_cols(ft)
    num_cols_unn <- num_cols[num_cols %in% unnested_cols]
    char_cols_unn <- char_cols[char_cols %in% unnested_cols]
    
    
    ft <- ft %>% 
      align(j = num_cols, align = "center",part = "header",i=1) %>%
      align(j = num_cols, align = "right",part = "header",i=2) %>%
      align(j = num_cols, align = "right",part = "body") %>%
      align(j = char_cols, align = "left",part = "header",i = 2) %>% 
      align(j = char_cols, align = "left",part = "body")
    
    if (length(num_cols_unn)>0){
      ft <- ft %>% 
        align( align = "right",part = "header",j =num_cols_unn )
    }
    
    if (length(char_cols_unn)>0){
      ft <- ft %>% 
        align(align = "left",part = "header",j =char_cols_unn )
    }
  } else {
    ft <- ft %>% 
      align(j = num_cols, align = "right",part = "all") %>%
      align(j = char_cols, align = "left",part = "all") 
  }
  ft %>% 
    padding(j = num_cols, padding.right = 4,part = "all") %>%    # small space on the right for numeric
    padding(j = char_cols, padding.left = 4,part = "all") 

}




produce_flextable2 <- function(elem=NULL,data_list=NULL,dep=NULL,amt=NULL,table,year){
  
  if (is.null(elem)){
    elem <- data_list[[dep]][[amt]][[table]]
  }
  
  total_row <- ifelse(is.null(elem$total_row),FALSE,TRUE)
  total_col <- ifelse(is.null(elem$total_col),FALSE,TRUE)
  turn_header <- ifelse(is.null(elem$turn_header),FALSE,TRUE)
  inner_border <- ifelse(is.null(elem$inner_border),FALSE,TRUE)
  
  if (elem$table_type=="nested_header"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year)
    
    template_data <- template_nested_header(data = temp_data,level1_col = elem$level1_col,level2_col = elem$level2_col,id_col = elem$id_col,val_col = elem$val_col,year_col = "jahr")
    
    ft <- create_flextable_nested_header(template_data,total_row,total_col)
  }
  
  if (elem$table_type=="none"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year) 
    
    if (!is.null(elem$new_names)){
      new_names <- elem$new_names %>% str_split(",") %>% unlist()
    }
    
    template_data <- template_none(data = temp_data,new_names = new_names,year_col = "jahr")
    
    ft <- create_flextable_simple(template_data,total_row,total_col)
    
  }
  
  if (elem$table_type=="nested_col"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year)
    
    if (!is.null(elem$new_names)){
      new_names <- elem$new_names %>% str_split(",") %>% unlist()
    }
    
    template_data <- template_nested_col(data = temp_data,level1_col = elem$level1_col,level2_col = elem$level2_col,colnames_col = elem$colnames_col,val_col = elem$val_col,year_col = "jahr",level1_name = elem$level1_name,level2_name = elem$level2_name)
    
    ft <- create_flextable_simple(template_data,total_row,total_col)
    
  }
  
  
  if (elem$table_type=="colname_wide_grouped"){
    
    temp_data <- elem$data %>% 
      filter(jahr == year)
    
    
    
    template_data <- template_colname_wide_grouped(data = temp_data,group_col = elem$group_col,label_col = elem$label_col,colnames_col = elem$colnames_col,val_col = elem$val_col,year_col = "jahr")
    
    ft <- create_flextable_simple(template_data,total_row,total_col)
    
    
    
    
  }
  
  if (turn_header) {
    
    add_invisible_line <- function(x) paste0(x, "\n\n")
    
    ft <- ft %>%
      rotate(rotation = "btlr", part = "header") %>%
      # align(align = "left", part = "body") %>% 
      # set_header_labels(Var1 = " " ) %>% 
      align(align = "left", part = "header") %>%
      height(height = 2, part = "header") %>%
      hrule(i = 1, rule = "exact", part = "header")
  }
  
  if (inner_border){
    ft <- ft %>% 
      flextable::border_inner(part = "body")
  }
  
  
  num_cols <- which(sapply( ft$body$dataset, is.numeric))
  char_cols <- which(sapply( ft$body$dataset, is.character))
  
  
  
  if (elem$table_type=="nested_header"){
    
    unnested_cols <- get_unnested_cols(ft)
    num_cols_unn <- num_cols[num_cols %in% unnested_cols]
    char_cols_unn <- char_cols[char_cols %in% unnested_cols]
    
    
    ft <- ft %>% 
      align(j = num_cols, align = "center",part = "header",i=1) %>%
      align(j = num_cols, align = "right",part = "header",i=2) %>%
      align(j = num_cols, align = "right",part = "body") %>%
      align(j = char_cols, align = "left",part = "header",i = 2) %>% 
      align(j = char_cols, align = "left",part = "body")
    
    if (length(num_cols_unn)>0){
      ft <- ft %>% 
        align( align = "right",part = "header",j =num_cols_unn )
    }
    
    if (length(char_cols_unn)>0){
      ft <- ft %>% 
        align(align = "left",part = "header",j =char_cols_unn )
    }
  } else {
    ft <- ft %>% 
      align(j = num_cols, align = "right",part = "all") %>%
      align(j = char_cols, align = "left",part = "all") 
  }
  ft <- ft %>% 
    padding(j = num_cols, padding.right = 4,part = "all") %>%    # small space on the right for numeric
    padding(j = char_cols, padding.left = 4,part = "all") %>% 
    fontsize(size = 11, part = "all")
    
  
  
  return(list(
    ft=ft,
    data = template_data
  ))
  
}
