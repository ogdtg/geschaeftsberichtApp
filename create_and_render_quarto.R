library(officer)
library(tidyverse)
library(flextable)
devtools::load_all()

create_and_render_quarto <- function(data_list,output_dir,year,output_word = "test.docx",app_dir = "/r-proj/stat/ogd/stat_anhang/geschaeftsberichtApp/",vorlagen_dir = "/r-proj/stat/ogd/stat_anhang/geschaeftsberichtApp/vorlagen/" ) {




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
          glue::glue("https://statistiktg.shinyapps.io/statanhang/?dept={dep_unique$dep[i]}&amt={amt_unique$amt[k]}&table={tables$table[j]}&year={year}")
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


create_and_render_quarto(data_list = nested_data_ogd,output_dir = "quarto_docs",output_word = "report_24_full.docx",year = 2024)


