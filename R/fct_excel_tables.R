
#' Create Excel Template (Flat Header)
#'
#' Generates an Excel file from a flat (non-nested) `data.frame`, including formatted headers, subheaders, and data styles.
#' The output file is saved in a nested directory structure based on department and amount.
#'
#' @param template_data A `data.frame` to be written into the Excel sheet.
#' @param sheet A character string naming the Excel sheet.
#' @param dep A string specifying the department name, used to organize file structure.
#' @param amt A string specifying the Amt name, used to organize file structure.
#' @param table_name The name of the table to be used as the file name.
#' @param path Base path where the Excel template should be saved. Defaults to `"excel_template"`.
#'
#' @return Writes an `.xlsx` file to disk. Returns no value.
#'
#' @importFrom openxlsx createWorkbook addWorksheet saveWorkbook setColWidths createStyle addStyle
#' @importFrom stringr str_wrap
#' @importFrom TGexcel create_header_style create_subheader_style create_varnames_style create_data_style add_style_feature
#' @export
create_excel_template <- function(template_data,sheet,dep,amt,table_name,path = "excel_template"){
  dir.create(paste0(path),showWarnings = F)
  dir.create(paste0(path,"/",dep),showWarnings = F)
  dir.create(paste0(path,"/",dep,"/",amt),showWarnings = F)

  wb <- createWorkbook()
  addWorksheet(wb,sheet)

  TGexcel::create_header_style(wb, sheet, ncol = ncol(template_data), text = str_wrap(table_name,width = 50))
  TGexcel::create_subheader_style(wb, sheet, ncol = ncol(template_data), text = paste0(dep,", ",amt,", ",sheet))
  TGexcel::create_varnames_style(wb,sheet,var_names = names(template_data))
  TGexcel::create_data_style(wb,sheet,data = template_data,startRow = 4)

  setColWidths(wb,sheet, cols = 2:nrow(template_data),widths = 25)
  setColWidths(wb,sheet, cols = 1,widths = 25)
  TGexcel::add_style_feature(wb,sheet,rows = 3:(nrow(template_data)+2),cols = 1,feature_names = "wrapText",feature_values = TRUE)
  bold_varnames  <- createStyle(textDecoration = "bold")
  addStyle(wb,sheet,rows = 3,cols = 1:ncol(template_data),style = bold_varnames,gridExpand = TRUE,stack = TRUE)


  saveWorkbook(wb,paste0(path,"/",dep,"/",amt,"/",table_name,".xlsx"),overwrite = T)
}




#' Create Excel Template (Nested Header)
#'
#' Exports a `data.frame` with nested column names (e.g., `"Group__Variable"`) into a formatted Excel file.
#' Applies custom styles for headers, subheaders, and nested column labels. Saves output in a structured directory.
#'
#' @param template_data A `data.frame` with nested column names (double underscores to separate levels).
#' @param sheet A string naming the Excel sheet.
#' @param dep Department name used to structure folder hierarchy.
#' @param amt Amount name used in folder structure.
#' @param table_name The filename to be used (excluding extension).
#' @param path Base folder for saving the Excel file. Defaults to `"excel_template"`.
#'
#' @return Writes a styled `.xlsx` file to disk. Returns no value.
#'
#' @importFrom openxlsx createWorkbook addWorksheet saveWorkbook setColWidths createStyle addStyle
#' @importFrom stringr str_wrap
#' @importFrom TGexcel create_header_style create_subheader_style create_data_style create_nested_header_style
#' @importFrom dplyr mutate select group_by ungroup distinct arrange mutate_at
#' @importFrom stringr str_split
#' @importFrom Hmisc capitalize
#' @export
create_excel_template_nested_header <- function(template_data,sheet,dep,amt,table_name,path = "excel_template"){
  dir.create(paste0(path),showWarnings = F)
  dir.create(paste0(path,"/",dep),showWarnings = F)
  dir.create(paste0(path,"/",dep,"/",amt),showWarnings = F)

  wb <- createWorkbook()
  addWorksheet(wb,sheet)

  TGexcel::create_header_style(wb, sheet, ncol = ncol(template_data), text = table_name)
  TGexcel::create_subheader_style(wb, sheet, ncol = ncol(template_data), text = paste0(dep,", ",amt,", ",sheet))

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

  TGexcel::create_nested_header_style(wb,sheet,vars_level1 =nesting_vec$group,vars_level2 = nested_names_df$label,nesting = nesting_vec$n )




  TGexcel::create_data_style(wb,sheet,data = template_data,startRow = 5)
  # add_style_feature(wb,sheet,rows = 3:(nrow(template_data)+2),cols = 1:ncol(template_data),feature_names = "wrapText",feature_values = TRUE)

  setColWidths(wb,sheet, cols = 2:nrow(template_data),widths = 25)
  setColWidths(wb,sheet, cols = 1,widths = 25)
  bold_varnames  <- createStyle(textDecoration = "bold")
  wraped_text <- createStyle(wrapText = TRUE)

  addStyle(wb,sheet,rows = 3:4,cols = 1:ncol(template_data),style = bold_varnames,gridExpand = TRUE,stack = TRUE)

  addStyle(wb,sheet,rows = 4:(nrow(template_data)+2),cols = 1:ncol(template_data),style = wraped_text,gridExpand = TRUE,stack = TRUE)


  saveWorkbook(wb,paste0(path,"/",dep,"/",amt,"/",table_name,".xlsx"),overwrite = T)
}
