read_aihw_shs_2 <- function(sheets){
  # sheets <- read_aihw("Specialist-homelessness-services-2020-21.xls",2,3)
  
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
