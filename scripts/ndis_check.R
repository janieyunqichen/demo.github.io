

rm(list = ls(all.names = TRUE))

library(dplyr)
library(tidyr)
library(stringr)


### Number of people with a psychosocial disability by age group (Mar 2023)
pre_n_agegrp <- read.csv("source/ndis-downloads/participant-numbers-and-average-plan-budgets/PB Participant Numbers and Plan Budgets data MAR23.csv") %>%
#pre_n_agegrp <- read.csv("source/ndis-downloads/participant-numbers-and-average-plan-budgets/PB Participant Numbers and Plan Budgets_MAR22_CSV.csv") %>%
  filter(DsbltyGrpNm == "Psychosocial disability") %>%
  filter(SuppClass == "ALL")  %>%
  filter(!StateCd %in% c("ALL", "OT")) %>%
  filter(!SrvcDstrctNm == "ALL") 

 
# table(pre_n_agegrp$StateCd, useNA = "ifany")
# table(pre_n_agegrp$AgeBnd, useNA = "ifany")
# table(pre_n_agegrp$RprtDt, useNA = "ifany")

nrow(pre_n_agegrp |> select(SrvcDstrctNm) |> distinct())

n_agegrp <- pre_n_agegrp  %>%
  mutate(people = ifelse(PrtcpntCnt == "<11", 3.7, as.numeric(PrtcpntCnt)),
         dollars = ifelse(AvgAnlsdCmtdSuppBdgt == "", 0, as.numeric(stringr::str_replace(AvgAnlsdCmtdSuppBdgt, ",", "")))) %>%
  mutate(
    year = stringr::str_sub(RprtDt, 8, 9),
    date = lubridate::mdy(paste0("Mar-31-20", year))
    ) %>%
  select(date, state = StateCd, AgeBnd, people, dollars) %>%
  group_by(date, state, AgeBnd) %>%
  summarise(people = sum(people), 
            dollars = sum(dollars),
            .groups = "drop") %>%
  mutate(agegrp = case_when(
    AgeBnd == "0 to 6" ~ "0006",
    AgeBnd == "7 to 14" ~ "0714",
    AgeBnd == "15 to 18" ~ "1518",
    AgeBnd == "19 to 24" ~ "1924",
    AgeBnd == "25 to 34" ~ "2534",
    AgeBnd == "35 to 44" ~ "3544",
    AgeBnd == "45 to 54" ~ "4554",
    AgeBnd == "55 to 64" ~ "5564",
    AgeBnd == "65+" ~ "65+",
    TRUE ~ "psy_total"
  )) %>%
  select(-c(AgeBnd, dollars)) %>%
  arrange(agegrp) %>%
  pivot_wider(names_from = agegrp, values_from = people, values_fill = 0) %>%
  pivot_longer(cols = c(`0006`:`65+`), values_to = "people", names_to = "agegrp")

table(n_agegrp$date, n_agegrp$state)
sum(n_agegrp$people)
# see estimate for Q3 FY 2223 at https://data.ndis.gov.au/explore-data


### Number of people with a disability by SA2
n_sa2 <- read.csv("source/ndis-downloads/PB Participants by SA2 data MAR23.csv") %>%
  mutate(day = stringr::str_sub(ReportDte, 1, 2),
         month = stringr::str_sub(ReportDte, 3, 5),
         year = stringr::str_sub(ReportDte, 6, 9),
    date = lubridate::dmy(paste0(day, "-", month, "-", year))) %>%
  select(date, state = StateCd, sa2 = SA2Cd2016, PrtcpntCnt) %>%
  filter(!state %in% c("OT", "MIS")) %>%
  #filter(!sa2 == "-") |> 
  mutate(n = ifelse(PrtcpntCnt == "<11", 5, as.numeric(PrtcpntCnt))) %>%
  group_by(date, state) %>%
  mutate(dis_total = sum(n, na.rm = T)) %>%
  ungroup() 

# Note the reduced number for 2023
table(n_sa2$date, n_sa2$state)

n_sa2_mar2023 <- n_sa2 %>% filter(date == lubridate::mdy("Mar-31-2023"))
sum(n_sa2_mar2023$n)


### Estimated number of people with a psychosocial disability by SA2 and agegrp
n_agebysa2 <- left_join(n_sa2_mar2023, n_agegrp, by = c("date", "state")) %>%
#n_agebysa2 <- left_join(n_sa2 %>% filter(state == "ACT"), n_agegrp %>% filter(state == "ACT"), by = "state") %>%
  mutate(
    # deflate n (i.e. people) to those with a psychosocial disability
    n_dash = n * psy_total/dis_total,
    # distribute the deflated n across the age groups (people_dash is the number we want.)
    people_dash = n_dash*people/psy_total
    ) 

sum(n_agebysa2$people_dash)

### This should be zero
nrow(n_agebysa2) - (9 * nrow(n_sa2_mar2023))




### Check with Rex's data
rex_data <- readRDS("data/NDIS/psyc-soc-participants-by-sa2.rds")  |> 
  ungroup() |> 
  filter(ReportDte == lubridate::mdy("Mar-31-2023")) 


sum(rex_data$count_0_to_6) + sum(rex_data$count_7_to_14) + sum(rex_data$count_15_to_18) + sum(rex_data$count_19_to_24) +
  sum(rex_data$count_25_to_34) + sum(rex_data$count_35_to_44) + + sum(rex_data$count_35_to_44) + sum(rex_data$count_55_to_64) +  sum(rex_data$`count_65+`)


rex_data_Mar2023 <- rex_data %>%
  pivot_longer(cols = starts_with("count_"), names_to = "AgeBnd", values_to = "rex_n") |> 
  mutate(
    AgeBnd = str_remove(AgeBnd, "count_"),
    agegrp = case_when(
      AgeBnd == "0_to_6" ~ "0006",
      AgeBnd == "7_to_14" ~ "0714",
      AgeBnd == "15_to_18" ~ "1518",
      AgeBnd == "19_to_24" ~ "1924",
      AgeBnd == "25_to_34" ~ "2534",
      AgeBnd == "35_to_44" ~ "3544",
      AgeBnd == "45_to_54" ~ "4554",
      AgeBnd == "55_to_64" ~ "5564",
      AgeBnd == "65+" ~ "65+",
      TRUE ~ "psy_total")
    ) |> 
  select(date = ReportDte, state = StateCd, sa2 = SA2Cd2016, SA2Nm2016, agegrp, rex_n)


# sum(rex_data_Mar2023$count_0_to_6) + sum(rex_data_Mar2023$count_7_to_14) + sum(rex_data_Mar2023$count_15_to_18) + sum(rex_data_Mar2023$count_19_to_24) +
#   sum(rex_data_Mar2023$count_25_to_34) + sum(rex_data_Mar2023$count_35_to_44) + sum(rex_data_Mar2023$count_55_to_64) +  sum(rex_data_Mar2023$`count_65+`)
  
  
sum(rex_data_Mar2023$rex_n)

compare <- full_join(rex_data_Mar2023, 
                     n_agebysa2 |> select(date, state, sa2, agegrp, people_dash), 
                     by = c("date", "state", "sa2", "agegrp"))


sum(compare$rex_n)
sum(compare$people_dash, na.rm = T) - sum(n_agegrp$people)




### Participants by state
names(n_agegrp)
state_pop_summary <-
  readRDS("/hpa/projects/r2023.206.unmet.need/data/SA2pop.rds") |> 
  mutate(agegrp = str_replace_all(agegrp, "-", "_")) |> 
  pivot_wider(names_from = agegrp, values_from = pop, names_prefix = "pop_") |> 
  mutate(
    pop_0011 = pop_0_4 + pop_5_9 + pop_10_14 * (2/5),
    pop_1224 = pop_10_14 * (3/5) + pop_15_19 + pop_20_24,
    pop_2564 = pop_25_29 + pop_30_34 + pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64,
    `pop_65+` = pop_65_69 + pop_70_74 + pop_75_79 + pop_80_84 + `pop_85+`,
    pop_total = pop_0011 + pop_1224 + pop_2564 + `pop_65+`
  ) |> 
  select(SA2, SA2name, sex, pop_0011, pop_1224, pop_2564, `pop_65+`, pop_total) |> 
  pivot_longer(
    starts_with("pop_"), 
    names_to = "agegrp", 
    values_to = "pop", 
    names_transform = \(x) str_replace(str_remove(x, "pop_"), "_", "-")
  ) |> 
  mutate(statenum = str_sub(SA2,1,1)) |> 
  mutate(state = case_when(
    statenum == 1 ~ "NSW",
    statenum == 2 ~ "VIC",
    statenum == 3 ~ "QLD",
    statenum == 4 ~ "SA",
    statenum == 5 ~ "WA",
    statenum == 6 ~ "TAS",
    statenum == 7 ~ "NT",
    statenum == 8 ~ "ACT",
    TRUE ~ "???"
  )) |> 
  group_by(agegrp, state) |> 
  summarise(pop = sum(pop))


table(state_pop_summary$state)


n_agegrp_summary <- n_agegrp |> 
  group_by(agegrp, state) |> 
  pivot_wider(names_from = agegrp, values_from = people, names_prefix = "p_") |> 
  mutate(
    p_0011 = p_0006 + p_0714 * (6/8),
    p_1224 = p_0714 * (2/8) + p_1518 + p_1924,
    p_2564 = p_2534 + p_3544 + p_4554 + p_5564,
    `p_65+` = `p_65+`,
    p_total = p_0011 + p_1224 + p_2564 + `p_65+`
  ) |> 
  select(state, p_0011, p_1224, p_2564, `p_65+`, p_total) |> 
  pivot_longer(cols = starts_with("p_"), names_to = "agegrp", values_to = "people") |> 
  mutate(agegrp = str_remove(agegrp, 'p_'))


n_state_summary <- full_join(n_agegrp_summary, state_pop_summary) |> 
  mutate(rate = round(people/pop * 100000, digits = 1)) |> 
  mutate(n_rate = paste0(prettyNum(round(people), big.mark = ","), " (", sprintf("%.1f", rate), ")")) |> 
  select(state, agegrp, n_rate) |> 
  pivot_wider(names_from = agegrp, values_from = n_rate)


write.csv(n_state_summary, "output/n_state_summary.csv")


names(n_agegrp)
national_pop_summary <-
  readRDS("/hpa/projects/r2023.206.unmet.need/data/SA2pop.rds") |> 
  mutate(agegrp = str_replace_all(agegrp, "-", "_")) |> 
  pivot_wider(names_from = agegrp, values_from = pop, names_prefix = "pop_") |> 
  mutate(
    pop_0011 = pop_0_4 + pop_5_9 + pop_10_14 * (2/5),
    pop_1224 = pop_10_14 * (3/5) + pop_15_19 + pop_20_24,
    pop_2564 = pop_25_29 + pop_30_34 + pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64,
    `pop_65+` = pop_65_69 + pop_70_74 + pop_75_79 + pop_80_84 + `pop_85+`,
    pop_total = pop_0011 + pop_1224 + pop_2564 + `pop_65+`
  ) |> 
  select(SA2, SA2name, sex, pop_0011, pop_1224, pop_2564, `pop_65+`, pop_total) |> 
  pivot_longer(
    starts_with("pop_"), 
    names_to = "agegrp", 
    values_to = "pop", 
    names_transform = \(x) str_replace(str_remove(x, "pop_"), "_", "-")
  ) |> 
  mutate(statenum = str_sub(SA2,1,1)) |> 
  mutate(state = case_when(
    statenum == 1 ~ "NSW",
    statenum == 2 ~ "VIC",
    statenum == 3 ~ "QLD",
    statenum == 4 ~ "SA",
    statenum == 5 ~ "WA",
    statenum == 6 ~ "TAS",
    statenum == 7 ~ "NT",
    statenum == 8 ~ "ACT",
    TRUE ~ "???"
  )) |> 
  group_by(agegrp, state) |> 
  summarise(pop = sum(pop))


table(state_pop_summary$state)


n_national_summary <- full_join(n_agegrp_summary, state_pop_summary) |> 
  filter(!agegrp == "total") |> 
  group_by(agegrp) |> 
  summarise(people = sum(people),
            pop = sum(pop),
            .groups = "drop") |> 
  mutate(rate = round(people/pop * 100000, digits = 1)) |> 
  mutate(n_rate = paste0(prettyNum(round(people), big.mark = ","), " (", sprintf("%.1f", rate), ")")) |> 
  select(agegrp, n_rate) |> 
  pivot_wider(names_from = agegrp, values_from = n_rate)





