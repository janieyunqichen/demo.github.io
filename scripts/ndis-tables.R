# ndis-tables.R
library(tidyverse)
library(flextable)
diag_counts <- readRDS(file.path(here::here(), "data/NDIS/other-participant-counts/participants-by-diagnosis.rds"))

diag_filt <- diag_counts |> 
  filter(
    NDISDsbltyGrpNm == "Psychosocial disability"
  )


f_fill_missing <- function(dat) {
  df_total <- dat |> 
    filter(ICDDsbltyNm == "All") |> 
    mutate(across(c("Reported", "Primary"), as.numeric))
  
  if(nrow(df_total) > 1) stop("bad total rows")
  
  df_cats <- dat |> 
    filter(ICDDsbltyNm != "All")
  
  n_small_num_reported <- sum(df_cats$Reported == "<11")
  n_small_num_primary <- sum(df_cats$Primary == "<11")
  
  values_reported_fill <- (df_total$Reported[1] - sum(as.numeric(df_cats$Reported), na.rm = TRUE)) / n_small_num_reported
  values_primary_fill <- (df_total$Primary[1] - sum(as.numeric(df_cats$Primary), na.rm = TRUE)) / n_small_num_primary
  
  dat$Primary[dat$Primary == "<11"] <- values_primary_fill
  dat$Reported[dat$Reported == "<11"] <- values_reported_fill
  
  dat |>
    mutate(across(c("Primary", "Reported"), as.numeric))
}


diag_filt |> 
  mutate(
    Secondary = as.numeric(Reported) - as.numeric(Primary),
    Secondary = ifelse(Reported == "<11", "<11", Secondary),
    Secondary = ifelse(is.na(Secondary), 
                       paste0(as.numeric(Reported) - 10, "-", as.numeric(Reported)),
                       Secondary)
  ) |> 
  write.csv(
    file.path(here::here(), "data/primary-and-secondary-diag-by-state-and-quarter.csv"),
    row.names = FALSE
  )


diag_counts |> 
  filter(
    RprtDt == "2023-03-31",
    StateCd == "All",
    NDISDsbltyGrpNm == "Psychosocial disability"
  ) |> 
  f_fill_missing() |> 
  select(ICDDsbltyNm, Primary, Reported) |> 
  (\(x) {
    rbind(
      x[x$ICDDsbltyNm == "All",],
      x[x$ICDDsbltyNm != "All",] |> arrange(ICDDsbltyNm)
    )
  })() |> 
  rename(`Disability name` = ICDDsbltyNm) |> 
  flextable()
  




