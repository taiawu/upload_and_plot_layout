# library(tidyverse) # needed for all data handling
# library(shinyalert) # needed for uploading

# genreal function to attempt actions
try_catch_popup <- function( execute_this, 
                             error_title = "Error!", 
                             error_subtitle = "Try, try again",
                             warning_title = "Warning!",
                             warning_subtitle = "You've been warned") {
  tryCatch( {
    execute_this
  }, warning = function(w) {
    shinyalert(warning_title, warning_subtitle)
  },  error = function(e) {
    shinyalert(error_title, error_subtitle)
  })
}

# function to read layouts
upload_layout <- function(filepath){
  # read file based on it's type
  ext <- tools::file_ext(filepath)
  raw <- switch(ext,
                csv = read_csv(filepath, col_names = FALSE),
                txt = read_tsv(filepath, col_names = FALSE),
                xlsx = readxl::read_excel(filepath, col_names = FALSE),
                xls = readxl::read_excel(filepath, col_names = FALSE)
                # ,
                # validate("Invalid file type")
  )
  
  # handle files with or without the "Type" header
  first_cell <- raw[1,1][[1]]
  out <- switch(first_cell,
                Type = raw[-1,],
                raw)
  
  # convert into layout form
  out %>%
    set_names( c("variable", "row", .[1,][-c(1,2)])) %>%
    filter(row %in% LETTERS[1:16]) %>%
    discard(~all(is.na(.x)))  %>% # drop columns if everything is NA
    filter_all(any_vars(!is.na(.))) %>% # drop empty rows
    mutate_all(as.character) %>% # make all character, to prevent issues in pivot
    pivot_longer(-c(variable, row), names_to = "column", values_to = "value") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(well = paste0(row, column)) %>% # make well column
    unite(condition, -c(row, column, well), sep = "__", remove = FALSE) %>% # make condition column
    filter(!across(-c(well, row, column, condition)) == "Empty") %>% # if all variables are "Empty",   wells
    filter(!is.na(across(-c(well, row, column, condition))) == TRUE) %>% # if all variables are NA,   wells
    mutate_all(parse_guess) # convert likely numeric variables to numeric
}

##### ---- working layout reading module
uploadLayoutUI <- function(id) {
  tagList(
    fileInput( NS(id, "file"), 
               label = "",
               placeholder = "Upload layout file" ,
               multiple = FALSE,
               accept = c(".csv", ".tsv", ".xls", ".xlsx")),
    tableOutput(NS(id,"layout_table"))
  )
}


uploadLayoutServer <- function(id) {
  moduleServer(id, function(input, output, session) { # data path comes from "input"
    
    layout_raw <- reactive({
      req(input$file)
      
      try_catch_popup(execute_this = upload_layout(input$file$datapath) ,
                      error_title = "Layout can't be read",
                      error_subtitle = "Please check layout format.")
      
    })
    
    output$layout_table <- renderTable({ layout_raw()  })
    reactive({layout_raw()}) # return this as the value for uploadLayoutServer
  })
  
}