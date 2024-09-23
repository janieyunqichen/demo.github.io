library(janitor)
library(zip)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

out_dir <- file.path(here::here(), "data", "AIHW")

# Download AIHW data
download_aihw <- function(link, data_file_name){
  folder_path <- "source/aihw-downloads"
  
  # Create the directory if it doesn't exist
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
  file_path <- file.path(folder_path, data_file_name)
  download.file(link, destfile = file_path, mode = "wb", quiet = FALSE)
}

# Download AIHW Data tables: Community mental health care services table 2020-21
download_aihw("https://www.aihw.gov.au/getmedia/16b85474-0993-444f-ac49-14ae1e132a8c/Community-mental-health-care-tables-2021.xlsx",
              "Community-mental-health-care-tables-2021.xlsx")

# Download AIHW Data tables: Specialist homelessness services 2020-21
download_aihw("https://www.aihw.gov.au/getmedia/087b7c19-7c40-4021-9ec1-b20fc92adb19/Specialist-homelessness-services-tables-2020-21.xls.aspx",
              "Specialist-homelessness-services-2020-21.xls")

# Download AIHW Data tables: Residential mental health care services 2020-21
download_aihw("https://www.aihw.gov.au/getmedia/4882ea3d-a40b-429a-998d-b243c55acf58/Residential-mental-health-care-tables-2020-21.xlsx.aspx",
              "Residential-mental-health-care-services-2020-21.xlsx")

# Download AIHW Data tables: Psychosocial disability support services 2020-21
download_aihw("https://www.aihw.gov.au/getmedia/176305a4-b05a-446f-8a7f-82c9b672e363/Psychosocial-disability-support-services-2021.xlsx.aspx",
              "Psychosocial-disability-support-services-2020-21.xlsx")


# For reading one sheet
read_sheet_aihw <- function(data_file_name, sheet_name, start_line){
  sheet <- readxl::read_excel(here::here("source/aihw-downloads", data_file_name), 
                              sheet = sheet_name,
                              skip = start_line) |>
    mutate(across(everything(), as.character))
}

# For reading the Table
read_aihw <- function(data_file_name, start_sheet, start_line) {
  folder_path <- "source/aihw-downloads"
  file <- paste0(folder_path,"/",data_file_name)
  sheet_names <- readxl::excel_sheets(file)[start_sheet:length(readxl::excel_sheets(file))]
  
  sheets <- purrr::map2(sheet_names, start_line, read_sheet_aihw, data_file_name = data_file_name)
  names(sheets) <- sheet_names
  
  return(sheets)
}


# Wrangling for Community-mental-health-care-tables-2021 sheet9
read_aihw_cmhc_9 <- function(){
  sheets <- read_aihw("Community-mental-health-care-tables-2021.xlsx", 4,4)
  sheet9 <- sheets[[9]]
  sheet9_na <- sheet9[!complete.cases(sheet9), ]
  sheet9 <- na.omit(sheet9)
  
  sheet9_clean <- pivot_longer(sheet9, NSW:Total,
                               names_to = "states_and_territories",
                               values_to = "value") |>
    mutate(
           value = as.numeric(value),
           table_code = "AIHW_Table_CMHC.9",
           table_name = "Community mental health care service patients, by demographic characteristics, states and territories, 2020–21",
           source = "Mental health services in Australia: State and territory community mental health services"
           ) |>
    clean_names() 

  # tidy count column
  sheet9_clean$count <- gsub("\\(|\\)","", sheet9_clean$count)
  sheet9_clean$count <- gsub(",","",sheet9_clean$count)
  sheet9_clean$count <- gsub("[^[:alnum:]]","_",sheet9_clean$count)
  
  # tidy demographic_variable column
  sheet9_clean$demographic_variable <- gsub("[^[:alnum:]()-]", " ", sheet9_clean$demographic_variable)
  sheet9_clean$demographic_variable <- gsub("(\\d) (\\d)", "\\1-\\2", sheet9_clean$demographic_variable)
  
  # add state levels
  state_levels <- c("NSW","Vic","Qld","WA","SA","Tas","ACT","NT","Total")
  
  sheet9_clean$states_and_territories <- factor(sheet9_clean$states_and_territories, 
                                                levels = state_levels)
  return(sheet9_clean)
    
}

# Save the RDS file 
read_aihw_cmhc_9() |>
  saveRDS(file.path(out_dir, "patients-by-demographic-states.rds"))

# Wrangling for Community-mental-health-care-tables-2021 sheet10
read_aihw_cmhc_10 <- function() {
  sheets <- read_aihw("Community-mental-health-care-tables-2021.xlsx", 4,4)
  sheet10 <- sheets[[10]]
  sheet10_na <- sheet10[!complete.cases(sheet10), ]
  sheet10 <- na.omit(sheet10)
  
  sheet10$`Age group` <- gsub("^Total.*", "Total", sheet10$`Age group`)
  sheet10$`Age group` <- gsub("^85.*over$", "85 years and over", sheet10$`Age group`)
  
  
  sheet10_clean <- sheet10 |> 
    pivot_longer(`2005–06`:`2020–21`,
                 names_to = "year",
                 values_to = "contacts_per1000_pop") |>
    clean_names() |>
    mutate(average_annual_change_per_cent_2016_17_to_2020_21 = as.numeric(average_annual_change_per_cent_2016_17_to_2020_21),
           contacts_per1000_pop = as.numeric(contacts_per1000_pop),
           table_code = "AIHW_Table_CMHC.10",
           table_name = "Community mental health care service contacts per 1,000 population, by sex and age group, 2005–06 to 2020–21",
           source = "Mental health services in Australia: State and territory community mental health services"
           )
  
  sheet10_clean$sex <- gsub("[^[:alpha:]]", "_", sheet10_clean$sex)
  sheet10_clean$sex <- gsub("All_persons", "All persons", sheet10_clean$sex)
  
  
  sheet10_clean$age_group <- gsub(" years.*","", sheet10_clean$age_group)
  sheet10_clean$age_group <- gsub("85","85+", sheet10_clean$age_group)
  
  age_group_levels <- c("Total", "0–4", "5–11", "12–17", "18–24", "25–34", "35–44", "45–54", "55–64", "65–74", "75–84", "85+")
  
  sheet10_clean$age_group <- factor(sheet10_clean$age_group, levels = age_group_levels)
  
  return(sheet10_clean)
}

# Save the RDS file 
read_aihw_cmhc_10() |>
  saveRDS(file.path(out_dir, "contacts-per1000-sex-age.rds"))


# Wrangling for Specialist homelessness services 2020-21 sheet2
read_aihw_shs_2 <- function(){
  sheets <- read_aihw("Specialist-homelessness-services-2020-21.xls",2,3)
  
  sheet <- sheets[[2]] %>% 
    select (-6)
  
  sheet_na <- sheet[!complete.cases(sheet), ]
  sheet <- na.omit(sheet)
  
  names(sheet) <- gsub("\\(.*?\\)", "", names(sheet)) 
  
  sheet <- sheet %>% 
    pivot_longer(cols = 3:8,
                 names_to = "measurement",
                 values_to = "value")
  
  sheet <- sheet %>% 
    mutate(mental_health_issue = ifelse(str_detect(measurement, "without"), "False", "True"),
           measurement = case_when(
             str_detect(measurement, "Number") ~ "Number",
             str_detect(measurement, "Per cent") ~ "Per cent",
             str_detect(measurement, "per") ~ "Rate (per 100,000 population)"),
           table_code = "AIHW_Table_SHS.2",
           table_name = "SHS clients with and without a current mental health issue, by demographic characteristics, 2020–21",
           source = "Mental health services in Australia—Specialist homelessness services"
           ) %>% 
    clean_names()
  
  sheet$group <- gsub("\\(.*?\\)", "", sheet$group) 
  
  sheet <- sheet %>%
    relocate(mental_health_issue, .after = client_demographics)
  
  sheet$value <- as.numeric(sheet$value)
  
  sheet_age <- sheet %>% 
    filter(group == "Age group")
  
  sheet_age$client_demographics <- gsub("[^[:alnum:]]", "-", sheet_age$client_demographics)
  sheet_age$client_demographics <- gsub("-years", "", sheet_age$client_demographics)
  sheet_age$client_demographics <- gsub("-and-over", "+", sheet_age$client_demographics)
  
  age_group_levels <- c("10-14", "15-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  sheet_age$client_demographics <- factor(sheet_age$client_demographics, levels = age_group_levels)
  
  sheet <- sheet %>% 
    filter(group != "Age group") %>% 
    bind_rows(sheet_age)
  
  return(sheet)
}

# Save the RDS file 
read_aihw_shs_2() |>
  saveRDS(file.path(out_dir, "shs2-shs-mental-health-issue-demographic.rds"))


# Wrangling for Residential mental health care sheet3

read_aihw_rmhc_3 <- function(){
  sheets <- read_aihw("Residential-mental-health-care-services-2020-21.xlsx",4,4)
  sheet <- sheets[[3]]
  sheet_na <- sheet[!complete.cases(sheet), ]
  sheet <- na.omit(sheet)
  
  sheet <- sheet %>% 
    pivot_longer(cols = 3:5,
                 names_to = "measurement",
                 values_to = "value") %>% 
    clean_names() %>% 
    mutate(table_code = "AIHW_Table_RMHC.3",
           table_name = "People accessing residential mental health care, by resident demographic characteristics, 2020–21",
           source = "Mental health services in Australia: Residential mental health care")
  
  sheet$value <- as.numeric(sheet$value)
  sheet$demographic <- gsub("[^[:alpha:]]", "_", sheet$demographic)
  
  sheet_age <- sheet %>% 
    filter(demographic == "Age_group")
  
  sheet_age$demographic_variable <- gsub("[^[:alnum:]]", "-", sheet_age$demographic_variable)
  sheet_age$demographic_variable <- gsub("-years", "", sheet_age$demographic_variable)
  sheet_age$demographic_variable <- gsub("-and-over", "+", sheet_age$demographic_variable)
  age_group_levels <- c("0-11", "12-17", "18-24", "25-34", "35-44", "45-54", "55+")
  sheet_age$demographic_variable <- factor(sheet_age$demographic_variable, levels = age_group_levels)
  
  sheet <- sheet %>% 
    filter(demographic != "Age_group") %>% 
    bind_rows(sheet_age)
  
  sheet$measurement <- gsub("[^[:alnum:]]", "_", sheet$measurement)
  sheet$measurement <- gsub("Rate__per_10_000__population_", "Rate (per 10,000 population)", sheet$measurement)
  sheet$measurement <- gsub("Per_cent", "Percentage", sheet$measurement)
  

  return(sheet)
}

read_aihw_rmhc_3()|>
  saveRDS(file.path(out_dir, "rmhc3-ppl-accessing-rmhc.rds"))

# Wrangling for Residential mental health care sheet4

read_aihw_rmhc_4 <- function(){
  sheets <- read_aihw("Residential-mental-health-care-services-2020-21.xlsx",4,4)
  sheet <- sheets[[4]]
  sheet_na <- sheet[!complete.cases(sheet), ]
  sheet <- na.omit(sheet)
  
  sheet <- pivot_longer(sheet, NSW:Total,
                        names_to = "states_and_territories",
                        values_to = "value") %>% 
    clean_names()
  
  # for age_group column
  sheet$age_group <- gsub("^Total.*", "Total", sheet$age_group)
  sheet$age_group <- gsub("[^[:alnum:]]", "-", sheet$age_group)
  sheet$age_group <- gsub("-years", "", sheet$age_group)
  sheet$age_group <- gsub("-and-over", "+", sheet$age_group)
  
  age_group_levels <- c("Total", "0-11", "12-17", "18-24", "25-34", "35-44", "45-54", "55+")
  sheet$age_group <- factor(sheet$age_group, levels = age_group_levels)
  
  # for sex column
  sheet$sex <- gsub("[^[:alpha:]]", "_", sheet$sex)
  sheet$sex <- gsub("All_persons", "All persons", sheet$sex)
  
  
  sheet$value <- as.numeric(sheet$value)
  
  sheet <- sheet %>% 
    mutate(table_code = "AIHW_Table_RMHC.4",
           table_name = "Residential mental health care episodes, by sex and age group, states and territories, 2020–21",
           source = "Mental health services in Australia: Residential mental health care")
  
  state_levels <- c("NSW","Vic","Qld","WA","SA","Tas","ACT","NT","Total")
  
  sheet$states_and_territories <- factor(sheet$states_and_territories, 
                                                levels = state_levels)
  return(sheet)
}

read_aihw_rmhc_4()|>
  saveRDS(file.path(out_dir, "rmhc4-rmhc-by-sex-age-states.rds"))


# Wrangling for Residential mental health care sheet6

read_aihw_rmhc_6 <- function(){
  sheets <- read_aihw("Residential-mental-health-care-services-2020-21.xlsx",4,4)
  sheet <- sheets[[6]]
  sheet_na <- sheet[!complete.cases(sheet), ]
  sheet <- na.omit(sheet)
  
  sheet <- sheet %>% 
    pivot_longer(`2005–06`:`2020–21`,
                 names_to = "year",
                 values_to = "episodes_per_10_000_pop") |>
    clean_names() 
  
  sheet$age_group <- gsub("^Total.*", "Total", sheet$age_group)
  sheet$age_group <- gsub("[^[:alnum:]]", "-", sheet$age_group)
  sheet$age_group <- gsub("-years", "", sheet$age_group)
  sheet$age_group <- gsub("-and-over", "+", sheet$age_group)
  
  age_group_levels <- c("Total", "0-11", "12-17", "18-24", "25-34", "35-44", "45-54", "55+")
  sheet$age_group <- factor(sheet$age_group, levels = age_group_levels)
  
  sheet$sex <- gsub("[^[:alpha:]]", "_", sheet$sex)
  sheet$sex <- gsub("All_persons", "All persons", sheet$sex)
  
  sheet$episodes_per_10_000_pop <- as.numeric(sheet$episodes_per_10_000_pop)
  sheet$average_annual_change_per_cent_2016_17_to_2020_21 <- as.numeric(sheet$average_annual_change_per_cent_2016_17_to_2020_21)
  
  sheet <- sheet %>% 
    mutate(table_code = "AIHW_Table_RMHC.6",
           table_name = "Residential mental health care episodes per 10,000 population, by sex and age group, 2005–06 to 2020–21",
           source = "Mental health services in Australia: Residential mental health care")
  return(sheet)
}

read_aihw_rmhc_6()|>
  saveRDS(file.path(out_dir, "rmhc6-rmhc-per-10000-pop-by-sex-n-age.rds"))


# Wrangling for Residential mental health care sheet7

read_aihw_rmhc_7 <- function(){
  sheets <- read_aihw("Residential-mental-health-care-services-2020-21.xlsx",4,4)
  sheet <- sheets[[7]]
  sheet_na <- sheet[!complete.cases(sheet), ]
  sheet <- na.omit(sheet)
  
  sheet <- sheet %>% 
    pivot_longer(`2005–06`:`2020–21`,
                 names_to = "year",
                 values_to = "value") %>% 
    clean_names() 
  
  sheet$demographic <- gsub("[^[:alpha:]]", "_", sheet$demographic)
  
  sheet$count <- gsub("[^[:alpha:]]", "_", sheet$count)
  sheet$count <- gsub("Rate__per________population_", "Rate_per_10_000_population", sheet$count)
  
  sheet_age <- sheet %>% 
    filter(demographic == "Age_group")
  
  sheet_age$demographic_variable <- gsub("[^[:alnum:]]", "-", sheet_age$demographic_variable)
  sheet_age$demographic_variable <- gsub("-years", "", sheet_age$demographic_variable)
  sheet_age$demographic_variable <- gsub("-and-over", "+", sheet_age$demographic_variable)
  age_group_levels <- c("0-11", "12-17", "18-24", "25-34", "35-44", "45-54", "55+")
  sheet_age$demographic_variable <- factor(sheet_age$demographic_variable, levels = age_group_levels)
  
  sheet <- sheet %>% 
    filter(demographic != "Age_group") %>% 
    bind_rows(sheet_age)
  
  sheet$value <- as.numeric(sheet$value)
  sheet$average_annual_change_per_cent_2016_17_to_2020_21 <- as.numeric(sheet$average_annual_change_per_cent_2016_17_to_2020_21)
  
  sheet <- sheet %>% 
    mutate(table_code = "AIHW_Table_RMHC.7",
           table_name = "Residential mental health care episodes, by resident demographic characteristics, 2005–06 to 2020–21",
           source = "Mental health services in Australia: Residential mental health care")
  
  return(sheet)
}

read_aihw_rmhc_7()|>
  saveRDS(file.path(out_dir, "rmhc7-rmhc-ts-by-age-sex.rds"))

## Wrangling for Psychosocial-disability-support-services-2020-21

read_aihw_dis_4 <- function(){
  sheets <- read_aihw("Psychosocial-disability-support-services-2020-21.xlsx",2,3)
  sheet <- sheets[[4]]
  sheet_na <- sheet[!complete.cases(sheet), ]
  sheet <- na.omit(sheet)
  
  sheet <- sheet %>% 
    clean_names()
  
  names(sheet) <- gsub("_b", "", names(sheet)) 
  sheet$participant_demographics <- gsub("[^[:alnum:]]", "_", sheet$participant_demographics)
  sheet$group <- gsub("[^[:alpha:]]", "_", sheet$group)
  
  sheet <- sheet %>%
    mutate_at(vars(number:rate_per_100_000_population), as.numeric) %>% 
    pivot_longer(cols = 3:5,
                 values_to = "value",
                 names_to = "measurement") %>% 
    mutate(table_code = "AIHW_Table_DIS.4",
           table_name = "National Disability Insurance Scheme (NDIS) active participants, psychosocial primary disability, by demographic characteristics, as at 31 December",
           source = "Mental health services in Australia - Psychosocial disability support services")
  
  sheet_age <- sheet %>% 
    filter(group == "Age_group")
  sheet_age$participant_demographics <- gsub("[^[:alnum:]]", "-", sheet_age$participant_demographics)
  sheet_age$participant_demographics <- gsub("-years", "", sheet_age$participant_demographics)
  sheet_age$participant_demographics <- gsub("-and-over", "+", sheet_age$participant_demographics)
  sheet_age$participant_demographics <- gsub("Less-than-", "<", sheet_age$participant_demographics)
  
  sheet <- sheet %>% 
    filter(group != "Age_group") %>% 
    bind_rows(sheet_age)
  
  sheet$measurement <- gsub("number", "Number", sheet$measurement)
  sheet$measurement <- gsub("per_cent", "Percentage", sheet$measurement)
  sheet$measurement <- gsub("rate_per_100_000_population", "Rate (per 100,000 population)", sheet$measurement)
  
  return(sheet)
}

read_aihw_dis_4()|>
  saveRDS(file.path(out_dir, "dis4-ndis-active-by-demographic.rds"))
