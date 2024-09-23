read_aihw_rmhc_6 <- function(sheets){
  # sheets <- read_aihw("Residential-mental-health-care-services-2020-21.xlsx",4,4)
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