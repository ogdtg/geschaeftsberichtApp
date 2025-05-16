library(openxlsx)
library(TGexcel)


create_excel_template <- function(template_data,sheet,dep,amt,table_name,path = "excel_template"){
  dir.create(paste0(path),showWarnings = F)
  dir.create(paste0(path,"/",dep),showWarnings = F)
  dir.create(paste0(path,"/",dep,"/",amt),showWarnings = F)
  
  wb <- createWorkbook()
  addWorksheet(wb,sheet)
  
  create_header_style(wb, sheet, ncol = ncol(template_data), text = str_wrap(table_name,width = 50))
  create_subheader_style(wb, sheet, ncol = ncol(template_data), text = paste0(dep,", ",amt,", ",sheet))
  create_varnames_style(wb,sheet,var_names = names(template_data))
  create_data_style(wb,sheet,data = template_data,startRow = 4)
  
  setColWidths(wb,sheet, cols = 2:nrow(template_data),widths = 25)
  setColWidths(wb,sheet, cols = 1,widths = 25)
  add_style_feature(wb,sheet,rows = 3:(nrow(template_data)+2),cols = 1,feature_names = "wrapText",feature_values = TRUE)
  bold_varnames  <- createStyle(textDecoration = "bold")
  addStyle(wb,sheet,rows = 3,cols = 1:ncol(template_data),style = bold_varnames,gridExpand = TRUE,stack = TRUE)
  
  
  saveWorkbook(wb,paste0(path,"/",dep,"/",amt,"/",table_name,".xlsx"),overwrite = T)
}





create_excel_template_nested_header <- function(template_data,sheet,dep,amt,table_name,path = "excel_template"){
  dir.create(paste0(path),showWarnings = F)
  dir.create(paste0(path,"/",dep),showWarnings = F)
  dir.create(paste0(path,"/",dep,"/",amt),showWarnings = F)
  
  wb <- createWorkbook()
  addWorksheet(wb,sheet)
  
  create_header_style(wb, sheet, ncol = ncol(template_data), text = table_name)
  create_subheader_style(wb, sheet, ncol = ncol(template_data), text = paste0(dep,", ",amt,", ",sheet))
  
  nested_names_df <- names(template_data) %>%
    str_split("__", simplify = T) %>%
    data.frame() %>%
    setNames(c("group", "label")) %>% 
    mutate(original_name = case_when(
      label==""~group,
      TRUE~paste0(group,"__",label)
    )) %>% 
    mutate(group = Hmisc::capitalize(group)) %>% 
    mutate(nr = 1:nrow(.)) %>% 
    arrange(nr) %>% 
    mutate_at(vars(c(label,group)),str_wrap)
  
  
  nesting_vec <- nested_names_df %>% 
    arrange(nr) %>% 
    group_by(group) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    select(group,n) %>% 
    distinct()
  
  create_nested_header_style(wb,sheet,vars_level1 =nesting_vec$group,vars_level2 = nested_names_df$label,nesting = nesting_vec$n )
  
  
  
  
  create_data_style(wb,sheet,data = template_data,startRow = 5)
  # add_style_feature(wb,sheet,rows = 3:(nrow(template_data)+2),cols = 1:ncol(template_data),feature_names = "wrapText",feature_values = TRUE)
  
  setColWidths(wb,sheet, cols = 2:nrow(template_data),widths = 25)
  setColWidths(wb,sheet, cols = 1,widths = 25)
  bold_varnames  <- createStyle(textDecoration = "bold")
  wraped_text <- createStyle(wrapText = TRUE)
  
  addStyle(wb,sheet,rows = 3:4,cols = 1:ncol(template_data),style = bold_varnames,gridExpand = TRUE,stack = TRUE)
  
  addStyle(wb,sheet,rows = 4:(nrow(template_data)+2),cols = 1:ncol(template_data),style = wraped_text,gridExpand = TRUE,stack = TRUE)
  
  
  saveWorkbook(wb,paste0(path,"/",dep,"/",amt,"/",table_name,".xlsx"),overwrite = T)
}