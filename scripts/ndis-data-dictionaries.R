# ndis-data-dictionaries.R

parts_numbers_and_plan_budgets_dict <- structure(list(Variable = c(
  "RprtDt", "StateCd", "SrvcDstrctNm",
  "DsbltyGrpNm", "AgeBnd", "SuppClass", "PrtcpntCnt", "AvgAnlsdCmtdSuppBdgt"
), Description = c(
  "Reporting date", "State/Territory where the participant resides",
  "Service district where the participant resides", "Disability group name of the participants primary reported disability",
  "Age band", "Support class", "Participant count", "Average annualised committed support "
)), class = "data.frame", row.names = c(NA, -8L))


plan_mngmt_types <- structure(list(Variable = c(
  "Rprt Date", "StateCd", "SrvcDstrctNm",
  "AgeBnd", "MngmntMthd", "DsbltyGrpNm ", "PlanCnt", "AvgAnlsdCmtdSuppBdgt"
), Description = c(
  "Reporting date", "State/Territory where the participant currently resides",
  "Service District where the participant currently resides", "Age band of the participant",
  "How the participant has chosen to manage their plan implementation",
  "Disability group of the participant’s primary reported disability",
  "Count of eligible plans", "Average Annualised Committed support "
)), class = "data.frame", row.names = c(NA, -8L))


parts_by_sa2 <- structure(list(Variable = c(
  "RprtDt", "StateCd", "SA2Cd2016",
  "SA2Nm2016", "PrtcpntCnt"
), Description = c(
  "Reporting date",
  "State/Territory where the participant resides", "SA2 2016 Code according to ABS classification",
  "SA2 2016 Name according to ABS classification", "Count of Participants"
)), class = "data.frame", row.names = c(NA, -5L))


parts_by_diagnosis <- structure(list(Variable = c(
  "RprtDt", "StateCd", "NDISDsbltyGrpNm",
  "ICDDsbltyNm", "Primary", "Reported"
), Description = c(
  "Reporting date",
  "State/Territory where the participant resides", "NDIS Disability group name of the participant’s primary reported disability",
  "International Classification of Diseases (ICD) code and name",
  "Number of participants where the disability was recorded as primary",
  "Number of participants who reported the disability (including primary or non-primary disabilities)"
)), class = "data.frame", row.names = c(NA, -6L))


parts_by_first_nations <- structure(list(Variable = c(
  "RprtDt", "StateCd", "MMMCd_2015",
  "•", "•", "•", "•", "•", "•", "PrtcpntCnt", "AvgAnlsdCmtdSuppBdgt"
), Description = c(
  "Reporting date", "State/Territory where the participant resides",
  "Modified Monash Model 2015 code for remoteness", "MM1: Major Cities",
  "MM2: Population > 50,000", "MM3: Population between 15,000 and 50,000",
  "MM4: Population between 5,000 and 15,000", "MM5: Population less than 5,000",
  "MM6 & 7: Remote and Very Remote", "Participant count", "Average annualised committed support"
)), class = "data.frame", row.names = c(NA, -11L)) |>
  dplyr::filter(Variable != "•")


parts_by_cald <- structure(list(Variable = c(
  "RprtDt", "StateCd", "MMMCd_2015",
  "•", "•", "•", "•", "•", "•", "PrtcpntCnt", "AvgAnlsdCmtdSuppBdgt"
), Description = c(
  "Reporting date ", "State/Territory where the participant resides ",
  "Modified Monash Model 2015 code for remoteness ", "MM1: Major Cities",
  "MM2: Population > 50,000", "MM3: Population between 15,000 and 50,000",
  "MM4: Population between 5,000 and 15,000", "MM5: Population less than 5,000",
  "MM6&7: Remote and Very Remote", "Participant count ", "Average annualised committed support"
)), class = "data.frame", row.names = c(NA, -11L)) |>
  dplyr::filter(Variable != "•")



l <- lapply(ls(), get)
names(l) <- ls()[ls() != "l"]

saveRDS(l, file.path(here::here(), "data", "NDIS", "data-dictionary.rds"))
