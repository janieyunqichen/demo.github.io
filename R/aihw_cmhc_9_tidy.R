# function for tidy the cmhc 9 table
read_aihw_cmhc_9 <- function(sheets){
  # sheets <- read_aihw("Community-mental-health-care-tables-2021.xlsx", 4,4)
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