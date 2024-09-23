
out_dir <- file.path(here::here(), "data", "ROGS")

# For downloading source csv data
download_rogs <- function(link, data_file_name){
  folder_path <- "source/rogs-downloads"
  
    if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
  file_path <- file.path(folder_path, data_file_name)
  download.file(link, destfile = file_path, mode = "wb", quiet = FALSE)
}

download_rogs("https://www.pc.gov.au/ongoing/report-on-government-services/2023/health/services-for-mental-health/rogs-2023-parte-section13-mental-health-dataset.csv",
              "rogs-13.csv")
download_rogs("https://www.pc.gov.au/ongoing/report-on-government-services/2023/health/services-for-mental-health/rogs-2023-parte-section13-services-for-mental-health-data-tables.xlsx",
              "rogs-13-for-reference.xlsx")


# For filtering the desired table
read_rogs_tab <- function(tab_no){
  sheet_raw <- read.csv(here::here("source/rogs-downloads", "rogs-13.csv"))
  
  sheet <- sheet_raw %>% 
    filter(Table_Number == paste0("13A.", tab_no)) %>% 
    mutate(table_code = paste0("ROGS_Table_13A.",tab_no),
           table_name = Description1,
           source = "Report on Government Services 2023 - 13 Services for mental health") %>% 
    select(-Description1)
  
  sheet_useful <- sheet[sapply(sheet, function(col) length(unique(col)) > 1)]
  return(sheet_useful)
}

read_rogs_tab(45) |>
  saveRDS(file.path(out_dir, "prevalence-among-adults-by-sex.rds"))

read_rogs_tab(46) |>
  saveRDS(file.path(out_dir, "prevalence-among-adults-by-age.rds"))

