# ndis-table-builder.R

# Objective:
#   - create a data.frame with the (quarterly) counts of participants accessing psychosocial care via the NDIS on an SA2 level by age group (12-24, 25-64, 65+)
#
# We have:
#   - (quarterly data) table of TOTAL NDIS users per SA2
#   - (quarterly data) table of Psychosocial care users and TOTAL users by state
#   - TOTAL population counts by SA2
#
# MUST:
#   - have the same quarterly total numbers within state between the aggregated across SA2 level and the state wide data (req handling over <11 cells)

library(tidyverse)

sa2_pop <- readRDS("/hpa/projects/r2023.206.unmet.need/data/SA2pop.rds")

ndis_users_sa2 <- readRDS(file.path(here::here(), "data", "NDIS", "participants-by-sa2.rds"))

ndis_users_all <- readRDS(file.path(
  here::here(), "data", "NDIS",
  "ndis-participant-numbers-and-plan-budgets.rds"
)) |>
  filter(DsbltyGrpNm == "ALL")

ndis_users_psy <- readRDS(file.path(
  here::here(), "data", "NDIS",
  "ndis-participant-numbers-and-plan-budgets.rds"
)) |>
  filter(DsbltyGrpNm == "Psychosocial disability")



f_fill <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.na(x)) {
    return(0)
  }
  if (str_detect(x, "<")) {
    return(NA_integer_)
  }
  return(as.numeric(x))
}

fill_small_cnts <- function(.data, total, max_cell_add = 10) {
  missing_n <- total - sum(as.numeric(.data$PrtcpntCnt), na.rm = TRUE)
  n_small_cells <- sum(is.na(as.numeric(.data$PrtcpntCnt)))

  cnt_fill <- min(c(missing_n / n_small_cells, max_cell_add))

  .data$PrtcpntCnt <- as.numeric(.data$PrtcpntCnt)
  .data$PrtcpntCnt[is.na(.data$PrtcpntCnt)] <- cnt_fill

  .data
}

f_test <- function(dt,
                   state,
                   supp_class = "ALL",
                   srvc = "ALL",
                   .browser_on_mismatch = FALSE) {
  ndis_users_psy_i <- ndis_users_psy |>
    filter(
      StateCd == state,
      RprtDt == dt,
      SuppClass == supp_class,
      SrvcDstrctNm == srvc
    )

  ndis_users_all_i <- ndis_users_all |>
    filter(
      StateCd == state,
      RprtDt == dt,
      SuppClass == supp_class,
      SrvcDstrctNm == srvc
    )


  ndis_users_sa2_i <- ndis_users_sa2 |>
    filter(ReportDte == dt, StateCd == state)

  sa2_ndis_user_sum <- sum(as.numeric(ndis_users_sa2_i$PrtcpntCnt))

  ndis_users_all_sum <- ndis_users_all_i |>
    filter(AgeBnd == "ALL") |>
    slice(1) |>
    pull(PrtcpntCnt) |>
    as.numeric() |>
    sum()


  if (any(str_detect((ndis_users_sa2_i$PrtcpntCnt), "<"))) {
    # if there are small cells in the SA2 (there is not "ALL" for SA2 data), then use the fill_small_cnts with the total from the ndis_users total.
    ndis_users_sa2_i <- fill_small_cnts(ndis_users_sa2_i, total = ndis_users_all_sum)
    sa2_ndis_user_sum <- sum(as.numeric(ndis_users_sa2_i$PrtcpntCnt))
  }

  if (sa2_ndis_user_sum != ndis_users_all_sum) {
    if (.browser_on_mismatch) browser()

    if (abs(sa2_ndis_user_sum - ndis_users_all_sum) < 100) {
      # if the difference between the sum of SA2 counts and the all users
      # from ndis - all is less than 10, randomly allocate the numbers to the sa2s.
      n_top <- 3
      change <- ndis_users_all_sum - sa2_ndis_user_sum
      sa2_code_top <- ndis_users_sa2_i |>
        arrange(desc(PrtcpntCnt)) |>
        slice(1:n_top) |>
        pull(SA2Cd2016)

      ndis_users_sa2_i$PrtcpntCnt[ndis_users_sa2_i$SA2Cd2016 %in% sa2_code_top] <- as.numeric(ndis_users_sa2_i$PrtcpntCnt[ndis_users_sa2_i$SA2Cd2016 %in% sa2_code_top]) + change / n_top

      sa2_ndis_user_sum <- sum(as.numeric(ndis_users_sa2_i$PrtcpntCnt))
    }
  }

  stopifnot(all.equal(sa2_ndis_user_sum, ndis_users_all_sum))

  ndis_users_psy_all_sum <- ndis_users_psy_i |>
    filter(AgeBnd == "ALL") |>
    slice(1) |>
    pull(PrtcpntCnt) |>
    as.numeric() |>
    sum()

  ndis_users_all_i <- filter(ndis_users_all_i, AgeBnd != "ALL") |> distinct()
  ndis_users_psy_i <- filter(ndis_users_psy_i, AgeBnd != "ALL") |> distinct()

  prop_psy_of_all <- ndis_users_psy_all_sum / ndis_users_all_sum


  # proportions of all ndis users that are recieving psychosocial care within each age band
  psy_age_props <-
    left_join(
      select(
        fill_small_cnts(ndis_users_all_i, total = ndis_users_all_sum),
        AgeBnd,
        all_prt_cnt = PrtcpntCnt
      ),
      select(
        fill_small_cnts(ndis_users_psy_i, total = ndis_users_psy_all_sum),
        AgeBnd,
        psy_prt_cnt = PrtcpntCnt
      ),
      by = "AgeBnd"
    ) |>
    rowwise() |>
    mutate(
      across(c("all_prt_cnt", "psy_prt_cnt"), \(x)replace_na(f_fill(x), 0)),
      prop_psy = psy_prt_cnt / all_prt_cnt
    ) |>
    select(AgeBnd, prop_psy) |>
    mutate(AgeBnd = str_replace_all(AgeBnd, " ", "_")) |>
    pivot_wider(names_from = AgeBnd, values_from = prop_psy, names_prefix = "prop_")

  cbind(ndis_users_sa2_i, psy_age_props) |>
    rowwise() |>
    mutate(across(starts_with("prop_"), \(x) f_fill(PrtcpntCnt) * x * prop_psy_of_all)) |>
    rename_with((\(x) str_replace(x, "prop", "count")), starts_with("prop_"))
}


f_do_state <- function(s) {
  unique(ndis_users_sa2$ReportDte) |>
    lapply(\(x) f_test(dt = x, state = s)) |>
    (\(x) do.call("rbind", x))()
}

ndis_psy_cnts_by_sa2 <-
  unique(ndis_users_sa2$StateCd)[!unique(ndis_users_sa2$StateCd) %in% c("OT", "MIS")] |>
  lapply(f_do_state) |>
  (\(x) do.call("rbind", x))()

ndis_psy_cnts_by_sa2 |> 
  ungroup() |> 
  ungroup() |> 
  # mutate(
  #   count_12_to_24 = count_0_to_6 + count_7_to_14 + count_15_to_18 + count_19_to_24,
  #   count_25_to_65 = count_25_to_34 + count_35_to_44 + count_45_to_54 + count_55_to_64
  # ) |> 
  # select(ReportDte:PrtcpntCnt, count_12_to_24, count_25_to_65, `count_65+`) |> 
  saveRDS(file.path(here::here(), "data", "NDIS", "psyc-soc-participants-by-sa2.rds"))
