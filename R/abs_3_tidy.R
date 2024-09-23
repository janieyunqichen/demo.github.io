
tidy_abs3 <- function(df){

  abs3_long <- df |>
    mutate(sex = str_extract(`Anxiety disorders`, "(PERSONS|MALE|FEMALE)"),
           age_group = str_extract(`...1`, "[0-9]{2}–[0-9]{2}"),
           age_group = str_replace(age_group, "–", "-"),
           name = str_extract(sheet, "[^_]+$"),
           age_group_type = case_when(
             age_group %in% c("16-34", "35-64", "65-85") ~ "type1",
             age_group %in% c("16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-85") ~ "type2",
             TRUE ~ NA_character_
           )) |>
    fill(sex, .direction = "down") |>
    filter(!is.na(age_group)) |>
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