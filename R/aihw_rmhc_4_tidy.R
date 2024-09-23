read_aihw_rmhc_4 <- function(sheets){
  # sheets <- read_aihw("Residential-mental-health-care-services-2020-21.xlsx",4,4)
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