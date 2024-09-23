read_aihw_cmhc_10 <- function(sheets) {
  # sheets <- read_aihw("Community-mental-health-care-tables-2021.xlsx", 4,4)
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