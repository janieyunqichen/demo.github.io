library(tidyverse)
out_dir <- file.path(here::here(), "data", "NDIS")


# NOTE: the I was getting 403 forbidden when trying to download the files straight from the website with download.file() so I just downloaded all the files and put them in source/ndis-downloads/*


# read participant numbers and average plan budgets CSV files
parts_and_budgets_dir <- file.path(
  here::here(), 
  "source", 
  "ndis-downloads",
  "participant-numbers-and-average-plan-budgets"
)


f_participants_and_budget_read.csv <- function(f) {
  read.csv(f) |> 
    mutate(
      across(any_of(c("PrtcpntCnt", "AvgAnlsdCmtdSuppBdgt")), as.character),
      RprtDt = tolower(RprtDt),
      RprtDt = str_remove(RprtDt, "-"),
      RprtDt = str_replace(RprtDt, "-", "20"),
      RprtDt = as.Date(RprtDt, "%d%b%Y")
    )
}


list.files(parts_and_budgets_dir, full.names = TRUE) |> 
  lapply(f_participants_and_budget_read.csv) |> 
  (\(x) do.call("bind_rows", x))() |> 
  saveRDS(file.path(out_dir, "ndis-participant-numbers-and-plan-budgets.rds"))
  

# read participants by SA2 CSV file
read.csv(
  file.path(
    here::here(), 
    "source", 
    "ndis-downloads",
    "PB Participants by SA2 data MAR23.csv"
  )
) |> 
  mutate(ReportDte = as.Date(ReportDte, "%d%b%Y")) |> 
  saveRDS(file.path(out_dir, "participants-by-sa2.rds"))


# read in plan management data
pm_dir <- parts_and_budgets_dir <- file.path(
  here::here(), 
  "source", 
  "ndis-downloads",
  "plan-management"
)

pm_files <- list.files(pm_dir, full.names = T)

for(zipped_file in pm_files[str_detect(pm_files, "zip$")]) {
  if(!unzip(zipped_file, list = TRUE)$Name %in% list.files(pm_files)) {
    unzip(zipped_file, exdir = pm_dir)
  }
}

data_files <- list.files(pm_dir, full.names = TRUE)
data_files <- data_files[str_detect(data_files, "csv$")]

f_plan_read.csv <- function(f) {
  dat <- read.csv(f) 
  
  f <- ifelse(str_detect(dat$RprtDt[1], "-"), "%d-%b-%y", "%d%b%Y")
  
  dat |>
    mutate(across(any_of(c("PlanCnt", "AvgAnlsdCmtdSuppBdgt")), as.character),
           RprtDt = as.Date(RprtDt, format=f))
}

data_files |> 
  lapply(f_plan_read.csv) |> 
  (\(x) do.call("bind_rows", x))() |> 
  saveRDS(file.path(out_dir, "plan-management-types.rds"))


# read in counts by diagnosis
other_data_dir <- file.path(
  here::here(), 
  "source", 
  "ndis-downloads",
  "other-participant-counts"
)

other_data_out_dir <- file.path(out_dir, "other-participant-counts")


f_other_read.csv <- function(f) {
  read.csv(file.path(other_data_dir, f))
}

f_other_read.csv("PB First Nations Participants MAR23.csv") |> 
  saveRDS(file.path(other_data_out_dir, "first-nations-participants.rds"))

f_other_read.csv("PB Culturally and linguistically diverse participants MAR23.csv") |> 
  saveRDS(file.path(other_data_out_dir, "cald-participants.rds"))

f_other_read.csv("PB Participants by Diagnosis data MAR23.csv") |> 
  mutate(RprtDt = as.Date(RprtDt, format = "%d%b%Y")) |> 
  saveRDS(file.path(other_data_out_dir, "participants-by-diagnosis.rds"))

read.csv("source/ndis-downloads/PB Participants by LGA data MAR23_0.csv") |> 
  mutate(ReportDt = as.Date(ReportDt, "%d%b%Y")) |> 
  saveRDS(file.path(here::here(), "data", "NDIS", "participants-by-lga.rds"))
