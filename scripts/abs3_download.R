library(zip)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

out_dir <- file.path(here::here(), "data", "ABS")

# Function to download NSMHW data from ABS website

download_abs <- function() {
  source <- "https://www.abs.gov.au/statistics/health/mental-health/national-study-mental-health-and-wellbeing/2020-21/Mental-health-tables-1-14.zip"
  temp <- tempfile()
  download.file(source, temp)
  zip::unzip(temp, exdir = "source/abs-downloads")
  
  return(0)
}

# download_abs()


# For reading the Table
read_abs <- function(tab_num, folder_path = "source/abs-downloads") {
  
  # For reading all sheets in one file
  read_sheet_abs <- function(sheet, file) {
    readxl::read_excel(file, sheet = sheet, skip = 5) %>% 
      mutate(across(everything(), as.character))
  }
  
  file_paths <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  file <- grep(sprintf("Table %s –", tab_num), file_paths, value = T)
  
  sheet_names <- readxl::excel_sheets(file)[2:length(readxl::excel_sheets(file))]
  sheets <- map(sheet_names, read_sheet_abs, file = file)
  names(sheets) <- sheet_names
  bind_rows(sheets, .id = "sheet") |>
    select(-sheet, sheet)
}

abs3 <- read_abs(3)

# ABS: Table 3 – 12-month mental disorders by age and sex
tidy_abs3 <- function(folder_path = "source/abs-downloads"){
  # Read ABS
  abs3 <- read_abs(3, folder_path = folder_path)
  abs3_na <- abs3[!complete.cases(abs3), ]
  
  abs3_long <- abs3 |>
    mutate(sex = str_extract(`Anxiety disorders`, "(PERSONS|MALE|FEMALE)"),
           age_group = if_else(
             !str_detect(`...1`, "Total"), 
             str_extract(`...1`, "[0-9]{2}–[0-9]{2}"), 
             str_extract(`...1`, "Total")),
           age_group = str_replace(age_group, "–", "-"),
           name = str_extract(sheet, "[^_]+$"),
           age_group_type = case_when(
             age_group %in% c("16-34", "35-64", "65-85") ~ "type1",
             age_group %in% c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-85") ~ "type2",
             TRUE ~ NA_character_
           )) |>
    fill(sex, .direction = "down") |>
    filter(!is.na(age_group) & !is.na(`Anxiety disorders`)) |>
    mutate(age_group_type = replace_na(age_group_type, "Total")) |>
    select(-c(`...1`, Total, sheet)) |>
    pivot_longer(cols = -c(sex, age_group, age_group_type, name),
                 names_to = "disorder_types") |>
    mutate(disorder_types = ifelse(disorder_types == "Any 12-month mental disorder(c)", "Any 12-month mental disorder", disorder_types),
           value = as.numeric(value),
           sex = str_to_title(sex),
           table_code = "ABS_Table3",
           table_name = "Persons 16–85 years, 12-month mental disorders by age and sex",
           source = "ABS National Study of Mental Health and Wellbeing: Summary Results, 2020–21"
           ) 
  
  # abs3_wide <- abs3_long |>
  #   pivot_wider() 
  
  return(abs3_long)
}

tidy_abs3() |> 
  saveRDS(file.path(out_dir, "abs-12-month-mental-disorders-by-age-and-sex.rds"))



### PMc
# abs4_percentbyseifa <- read_abs(4) |> 
#   slice(227:232) 
# 
# write.csv(abs4_percentbyseifa, "output/abs4_percentbyseifa.csv")



