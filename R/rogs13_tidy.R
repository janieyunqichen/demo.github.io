# For filtering the desired table
read_rogs_tab <- function(file_path, tab_no){
  sheet_raw <- read.csv(file_path)
  
  sheet <- sheet_raw %>% 
    filter(Table_Number == paste0("13A.", tab_no)) %>% 
    mutate(table_code = paste0("ROGS_Table_13A.",tab_no),
           table_name = Description1,
           source = "Report on Government Services 2023 - 13 Services for mental health") %>% 
    select(-Description1)
  
  sheet_useful <- sheet[sapply(sheet, function(col) length(unique(col)) > 1)]
  return(sheet_useful)
}