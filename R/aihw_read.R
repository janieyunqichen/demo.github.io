
# For reading the Table
read_aihw <- function(file_path, start_sheet, start_line) {
  
  # For reading one sheet
  read_sheet_aihw <- function(file_path, sheet_name, start_line){
    sheet <- readxl::read_excel(file_path, 
                                sheet = sheet_name,
                                skip = start_line) |>
      mutate(across(everything(), as.character))
  }
  
  #folder_path <- "source/aihw-downloads"
  #file <- paste0(folder_path,"/",data_file_name)
  sheet_names <- readxl::excel_sheets(file_path)[start_sheet:length(readxl::excel_sheets(file_path))]
  
  sheets <- purrr::map2(sheet_names, start_line, read_sheet_aihw, file_path = file_path)
  names(sheets) <- sheet_names
  
  return(sheets)
}
