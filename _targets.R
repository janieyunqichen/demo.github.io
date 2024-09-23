library(targets)

# Load the R scripts in R folder with custom functions
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)


# Set target options
tar_option_set(
  packages = c("ggplot2",
               "zip",
               "readxl",
               "dplyr",
               "janitor",
               "stringr",
               "tidyr",
               "purrr"),
  format = "rds"
)


# Define the pipeline
list(
  # ABS data
  tar_target(abs_download, download_abs()),
  tar_target(abs_tab3_raw, read_abs(abs_download, tab_num=3)),
  tar_target(abs_tab3, tidy_abs3(abs_tab3_raw)),
  # tar_target(abs_tab3_wide, abs_tab3 |> pivot_wider()), # wider output
  
  # AIHW source data
  # CMHC 
  tar_target(aihw_cmhc_download, download_aihw("https://www.aihw.gov.au/getmedia/16b85474-0993-444f-ac49-14ae1e132a8c/Community-mental-health-care-tables-2021.xlsx",
                                               "Community-mental-health-care-tables-2021.xlsx")),
  tar_target(aihw_cmhc_raw, read_aihw(aihw_cmhc_download, start_sheet=4, start_line=4)),
  tar_target(aihw_cmhc_9, read_aihw_cmhc_9(aihw_cmhc_raw)),
  tar_target(aihw_cmhc_10, read_aihw_cmhc_10(aihw_cmhc_raw)),
  
  # SHS 
  tar_target(aihw_shs_download, download_aihw("https://www.aihw.gov.au/getmedia/087b7c19-7c40-4021-9ec1-b20fc92adb19/Specialist-homelessness-services-tables-2020-21.xls.aspx",
                                              "Specialist-homelessness-services-2020-21.xls")),
  tar_target(aihw_shs_raw, read_aihw(aihw_shs_download, start_sheet=2, start_line=3)),
  tar_target(aihw_shs_2, read_aihw_shs_2(aihw_shs_raw)),
  
  # RMHC 
  tar_target(aihw_rmhc_download, download_aihw("https://www.aihw.gov.au/getmedia/4882ea3d-a40b-429a-998d-b243c55acf58/Residential-mental-health-care-tables-2020-21.xlsx.aspx",
                                               "Residential-mental-health-care-services-2020-21.xlsx")),
  tar_target(aihw_rmhc_raw, read_aihw(aihw_rmhc_download, start_sheet=4, start_line=4)),
  tar_target(aihw_rmhc_3, read_aihw_rmhc_3(aihw_rmhc_raw)),
  tar_target(aihw_rmhc_4, read_aihw_rmhc_4(aihw_rmhc_raw)),
  tar_target(aihw_rmhc_6, read_aihw_rmhc_6(aihw_rmhc_raw)),
  
  # DIS
  tar_target(aihw_dis_download, download_aihw("https://www.aihw.gov.au/getmedia/176305a4-b05a-446f-8a7f-82c9b672e363/Psychosocial-disability-support-services-2021.xlsx.aspx",
                                              "Psychosocial-disability-support-services-2020-21.xlsx")),
  tar_target(aihw_dis_raw, read_aihw(aihw_dis_download, start_sheet=2, start_line=3)),
  tar_target(aihw_dis_4, read_aihw_dis_4(aihw_dis_raw)),
  
  # ROGS13
  tar_target(rogs13_download, download_rogs("https://www.pc.gov.au/ongoing/report-on-government-services/2023/health/services-for-mental-health/rogs-2023-parte-section13-mental-health-dataset.csv",
                                            "rogs-13.csv")),
  tar_target(rogs13_45,read_rogs_tab(rogs13_download, 45)),
  tar_target(rogs13_46,read_rogs_tab(rogs13_download, 46))
)



## Check the functions
# tar_manifest(fields = command)

## Check the flow
# tar_visnetwork()

# tar_make()

# # Run the target functions
# tar_read("aihw_cmhc_10")


# tar_invalidate(read_sheet_aihw)