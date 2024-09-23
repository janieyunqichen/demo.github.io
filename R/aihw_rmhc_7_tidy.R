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