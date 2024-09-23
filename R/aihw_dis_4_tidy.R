read_aihw_dis_4 <- function(sheets){
  # sheets <- read_aihw("Psychosocial-disability-support-services-2020-21.xlsx",2,3)
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