read_abs <- function(file_paths, tab_num) {
  # change to function(file_paths, tab_num)
  # For reading all sheets in one file
  read_sheet_abs <- function(sheet, file) {
    readxl::read_excel(file, sheet = sheet, skip = 5) %>% 
      mutate(across(everything(), as.character))
  }
  
  # comment out below:
  #file_paths <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  file <- grep(sprintf("Table %s –", tab_num), file_paths, value = T)
  
  sheet_names <- readxl::excel_sheets(file)[2:length(readxl::excel_sheets(file))]
  sheets <- map(sheet_names, read_sheet_abs, file = file)
  names(sheets) <- sheet_names
  df <-bind_rows(sheets, .id = "sheet") |>
    select(-sheet, sheet)
  
  return(df)
}
