read_aihw_rmhc_3 <- function(sheets){
  # sheets <- read_aihw("Residential-mental-health-care-services-2020-21.xlsx",4,4)
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