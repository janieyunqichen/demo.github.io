### Check with Rex's data
library(tidyverse)
library(sf)
library(hpa.spatial) # devtools::install("../../packages/hpa.spatial/")

sa2_geo <- strayr::read_absmap("sa22021")

sa2_pop <-
  readRDS("/hpa/projects/r2023.206.unmet.need/data/SA2pop.rds") |> 
  mutate(agegrp = str_replace_all(agegrp, "-", "_")) |> 
  pivot_wider(names_from = agegrp, values_from = pop, names_prefix = "pop_") |> 
  mutate(
    pop_0_11 = pop_0_4 + pop_5_9 + pop_10_14 * (2/5),
    pop_12_24 = pop_10_14 * (3/5) + pop_15_19 + pop_20_24,
    pop_25_64 = pop_25_29 + pop_30_34 + pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64,
    `pop_65+` = pop_65_69 + pop_70_74 + pop_75_79 + pop_80_84 + `pop_85+`,
    pop_all = pop_0_11 + pop_12_24 + pop_25_64 + `pop_65+`
  ) |> 
  select(SA2, SA2name, sex, pop_0_11, pop_12_24, pop_25_64, `pop_65+`, pop_all) |> 
  pivot_longer(
    starts_with("pop_"), 
    names_to = "agegrp", 
    values_to = "pop", 
    names_transform = \(x) str_replace(str_remove(x, "pop_"), "_", "-")
  ) |> 
  
  # combine males and females
  group_by(SA2, SA2name, agegrp) |> 
  summarize(pop = sum(pop)) |> 
  ungroup()

sa2_pop |> plyr::count("agegrp")



psyc_soc_sa2 <- readRDS("data/NDIS/psyc-soc-participants-by-sa2.rds") |> 
  filter(ReportDte == "2023-03-31") |> 
  select(SA2Cd2016, starts_with("count_")) |> 
  pivot_longer(!SA2Cd2016, names_to = "agegrp", values_to = "count") |> 
  filter(SA2Cd2016 != "-")

psyc_soc_sa2_2021 <- map_data_with_correspondence(
  codes = psyc_soc_sa2$SA2Cd2016, 
  values = psyc_soc_sa2$count, 
  groups = psyc_soc_sa2$agegrp,
  from_area = "sa2",
  from_year = 2016,
  to_area = "sa2",
  to_year = "2021.csv",
  value_type = "aggs"
) |> 
  pivot_wider(names_from = "grp", values_from = "values")


ndis_by_sa2 <-
  sa2_geo |> 
  select(sa2_code_2021) |> 
  left_join(
    psyc_soc_sa2_2021, 
    by = c("sa2_code_2021" = "SA2_CODE_2021")
  ) |> 
  (\(x) left_join(select(sa2_geo, sa2_code_2021, state_name_2021), as.data.frame(x)))() |> 
  mutate(
    state = toupper(strayr::clean_state(state_name_2021)),
    # assume uniform distribution of participants between ages 7 and 14 for splitting into agegrp's
    count_0_to_11 = count_0_to_6 + count_7_to_14 * (5/8),
    count_12_to_24 = count_7_to_14 * (3/8) + count_15_to_18 + count_19_to_24,
    count_25_to_64 = count_25_to_34 + count_35_to_44 + count_45_to_54 + count_55_to_64,
    count_all = count_0_to_11 + count_12_to_24 + count_25_to_64 + `count_65+`
  ) |> 
  select(-any_of(c(
    "count_0_to_6", "count_7_to_14", "count_15_to_18", "count_19_to_24",
    "count_25_to_34", "count_35_to_44", "count_45_to_54", "count_55_to_64"
  ))) |> 
  pivot_longer(starts_with("count_"), names_to = "agegrp", values_to = "count") |> 
  filter(state != "OTH") |> 
  mutate(
    agegrp = str_replace_all(str_extract(agegrp, "(?<=_).*"), "_to_", "-"),
    agegrp = factor(
      agegrp, 
      levels = c(
        "0-11",
        "12-24",
        "25-64",
        "65+",
        "all"
      )
    )
  ) |> 
  mutate(sa2_code_2021 = as.numeric(sa2_code_2021)) |> 
  filter(!is.na(sa2_code_2021))

sa2_geo_agegrp_grid <- expand.grid(
  sa2_code_2021 = sa2_geo$sa2_code_2021,
  agegrp = levels(ndis_by_sa2$agegrp)#,
  # ReportDte = unique(ndis_by_sa2$ReportDte)
) |> 
  (\(x) left_join(select(sa2_geo, sa2_code_2021), x))() |> 
  mutate(sa2_code_2021 = as.numeric(sa2_code_2021))


ndis_by_sa2_with_rate <- ndis_by_sa2 |> 
  left_join(
    select(sa2_pop, -SA2name), 
    by = c("sa2_code_2021" = "SA2", "agegrp")
  ) |> 
  mutate(rate_per_100k = count / (pop / 100000)) |> 
  as.data.frame() |> 
  select(-geometry) |> 
  (\(x) left_join(sa2_geo_agegrp_grid, x, by = c("sa2_code_2021", "agegrp"#, "ReportDte"
                                                 )))()


map_theme <- list(
  theme_bw()
)


ndis_by_sa2_with_rate |> 
  filter(#ReportDte == "2023-03-31",
         agegrp %in% c("25-64", "65+")) |> 
  ggplot() +
  geom_sf(aes(fill = rate_per_100k), lwd = 0.05) +
  facet_grid(vars(agegrp)#, vars(ReportDte)
             ) +
  scale_fill_gradientn(colours = c("lightyellow", "yellow", "orange", "darkorange", "red", "darkred", "black")) +
  labs(fill = "Number of participants\nper 100,000 population") +
  map_theme



ndis_by_sa2_with_rate |> 
  filter(#ReportDte == "2023-03-31",
         agegrp %in% c("25-64", "65+")) |> 
  ggplot() +
  geom_sf(aes(fill = count), lwd = 0.05) +
  facet_grid(vars(agegrp)#, vars(ReportDte)
             ) +
  scale_fill_gradientn(colours = c("lightyellow", "yellow", "orange", "darkorange", "red", "darkred", "black")) +
  labs(fill = "Number of participants") +
  map_theme


###### maps requested on 2023-08-08 ######

# 25-64 agegrp rates by SA2 and SA3 in QLD only
make_sa2_sa3_maps <- function(.data, state_keep = "QLD", agegrp_keep = "25-64", fill_missing_counts_as_0 = TRUE) {
  sa2_data <- 
    .data |> 
    filter(agegrp %in% agegrp_keep, state %in% state_keep)
  
  if(fill_missing_counts_as_0) {
    sa2_data <- sa2_data |> 
      mutate(
        rate_per_100k = ifelse(is.na(count) & !is.na(pop), 0, rate_per_100k),
        count = ifelse(is.na(count) & !is.na(pop), 0, count)
      )
  }
  
  # browser()
  sa3_geo <- strayr::read_absmap("sa32021") |> 
    filter(toupper(strayr::clean_state(state_name_2021)) %in% state_keep) |> 
    select(sa3_code_2021)
  
  sa3_pop <- with(
    sa2_pop,
    map_data_with_correspondence(
      codes = SA2,
      values = pop,
      groups = agegrp,
      from_area = "sa2",
      to_area = "sa3",
      from_year = 2021,
      to_year = 2021,
      value_type = "aggs"
    )
  ) |> 
    rename(pop = values)
  
  sa3_count <- with(
    na.omit(sa2_data),
    map_data_with_correspondence(
      codes = sa2_code_2021,
      values = count,
      groups = agegrp,
      from_area = "sa2",
      to_area = "sa3",
      from_year = 2021,
      to_year = 2021,
      value_type = "aggs"
    )
  ) |> 
    rename(count = values)
  
  
  sa3_data <- inner_join(sa3_pop, sa3_count, by = c("sa3_code_2021", "grp")) |> 
    (\(x) left_join(sa3_geo, x, by = "sa3_code_2021"))() |> 
    mutate(rate_per_100k = count / (pop / 100000))
  
  rbind(
    select(sa2_data, sa_code = sa2_code_2021, rate_per_100k) |> add_column(SA = "SA2"),
    select(sa3_data, sa_code = sa3_code_2021, rate_per_100k) |> add_column(SA = "SA3")
  ) |> 
    ggplot() +
    geom_sf(aes(fill = rate_per_100k), lwd = 0.05) +
    scale_fill_gradientn(colours = c("lightyellow", "yellow", "orange", "darkorange", "red", "darkred", "black")) +
    facet_wrap(~SA) +
    labs(fill = "Number of participants\nper 100,000 population") +
    map_theme +
    theme(legend.position = "bottom")
  
}

make_sa2_sa3_maps(ndis_by_sa2_with_rate, state_keep = "NSW")
make_sa2_sa3_maps(
  ndis_by_sa2_with_rate, 
  state_keep = strayr::state_abb_au
)

make_sa3_and_hhs <- function() {
  sa3_qld <- get_polygon("sa32021") |> 
    st_transform(7844) |> 
    filter(toupper(strayr::clean_state(state_name_2021)) == "QLD")
  
  hhs_qld <- get_polygon("HHS")
  
  rbind(
    sa3_qld |> select() |> mutate(geo = "SA3"),
    hhs_qld |> select() |> mutate(geo = "HHS")
  ) |> 
    ggplot() +
    geom_sf(aes(linetype = geo, col = geo), alpha = 0.5) +
    # facet_wrap(~geo) +
    map_theme +
    labs(col = "", linetype = "")
  
}

make_sa3_and_hhs()


### trends graphs

# psyc_soc_sa2 <- 
psyc_soc_sa2_all <- readRDS("data/NDIS/psyc-soc-participants-by-sa2.rds") |> 
  # filter(ReportDte == "2023-03-31") |> 
  select(SA2Cd2016, ReportDte, starts_with("count_")) |> 
  pivot_longer(starts_with("count_"), names_to = "agegrp", values_to = "count") |> 
  filter(SA2Cd2016 != "-")


psyc_soc_sa2_2021_all <-
  lapply(
    unique(psyc_soc_sa2_all$ReportDte),
    \(x) {
      dat <- filter(psyc_soc_sa2_all, ReportDte == x)
      
      map_data_with_correspondence(
        codes = dat$SA2Cd2016, 
        values = dat$count, 
        groups = dat$agegrp,
        from_area = "sa2",
        from_year = 2016,
        to_area = "sa2",
        to_year = "2021.csv",
        value_type = "aggs"
      ) |> 
        pivot_wider(names_from = "grp", values_from = "values") |> 
        add_column(ReportDte = x)
    }
  )

psyc_soc_sa2_2021_all_df <- do.call("rbind", psyc_soc_sa2_2021_all)

ndis_by_sa2_all <-
  sa2_geo |> 
  select(sa2_code_2021) |> 
  left_join(
    psyc_soc_sa2_2021_all_df, 
    by = c("sa2_code_2021" = "SA2_CODE_2021")
  ) |> 
  (\(x) left_join(select(sa2_geo, sa2_code_2021, state_name_2021), as.data.frame(x)))() |> 
  mutate(
    state = toupper(strayr::clean_state(state_name_2021)),
    # assume uniform distribution of participants between ages 7 and 14 for splitting into agegrp's
    count_0_to_11 = count_0_to_6 + count_7_to_14 * (5/8),
    count_12_to_24 = count_7_to_14 * (3/8) + count_15_to_18 + count_19_to_24,
    count_25_to_64 = count_25_to_34 + count_35_to_44 + count_45_to_54 + count_55_to_64,
    count_all = count_0_to_11 + count_12_to_24 + count_25_to_64 + `count_65+`
  ) |> 
  select(-any_of(c(
    "count_0_to_6", "count_7_to_14", "count_15_to_18", "count_19_to_24",
    "count_25_to_34", "count_35_to_44", "count_45_to_54", "count_55_to_64"
  ))) |> 
  pivot_longer(starts_with("count_"), names_to = "agegrp", values_to = "count") |> 
  filter(state != "OTH") |> 
  mutate(
    agegrp = str_replace_all(str_extract(agegrp, "(?<=_).*"), "_to_", "-"),
    agegrp = factor(
      agegrp, 
      levels = c(
        "0-11",
        "12-24",
        "25-64",
        "65+",
        "all"
      )
    )
  ) |> 
  mutate(sa2_code_2021 = as.numeric(sa2_code_2021)) |> 
  filter(!is.na(sa2_code_2021))

sa2_geo_agegrp_grid_all <- expand.grid(
  sa2_code_2021 = sa2_geo$sa2_code_2021,
  agegrp = levels(ndis_by_sa2_all$agegrp),
  ReportDte = na.omit(unique(ndis_by_sa2_all$ReportDte))
) |> 
  (\(x) left_join(select(sa2_geo, sa2_code_2021), x))() |> 
  mutate(sa2_code_2021 = as.numeric(sa2_code_2021))


ndis_by_sa2_with_rate_all <- ndis_by_sa2_all |> 
  left_join(
    select(sa2_pop, -SA2name), 
    by = c("sa2_code_2021" = "SA2", "agegrp")
  ) |> 
  mutate(rate_per_100k = count / (pop / 100000)) |> 
  as.data.frame() |> 
  select(-geometry) |> 
  (\(x) left_join(sa2_geo_agegrp_grid_all, x, by = c("sa2_code_2021", "agegrp", "ReportDte"
  )))()

sa2_data <- sa2_data |> 
  mutate(
    rate_per_100k = ifelse(is.na(count) & !is.na(pop), 0, rate_per_100k),
    count = ifelse(is.na(count) & !is.na(pop), 0, count)
  )

ndis_trends_data <- 
  ndis_by_sa2_with_rate_all |> 
  mutate(
    rate_per_100k = ifelse(is.na(count) & !is.na(pop), 0, rate_per_100k),
    count = ifelse(is.na(count) & !is.na(pop), 0, count)
  ) |> 
  as.data.frame() |> 
  select(-geometry)
  


state_order <- ndis_trends_data |> 
  mutate(num = str_extract(sa2_code_2021, "[0-9]")) |> 
  select(num, state) |> 
  distinct() |> 
  na.omit() |> 
  pull(state)

ndis_trends_data |> 
  filter(agegrp == "all") |> 
  filter(!is.na(count), !is.na(pop)) |> 
  group_by(state, ReportDte) |> 
  summarize(count = sum(count), pop = sum(pop)) |> 
  ungroup() |> 
  mutate(
    rate_per_100k = count / (pop / 100000),
    state = factor(state, levels = state_order)
  ) |> 
  ggplot(aes(ReportDte, rate_per_100k, col = state)) +
  geom_point() +
  geom_line() + 
  scale_x_date(date_labels = "%b %Y", breaks = unique(ndis_trends_data$ReportDte)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Quarter",
    y = "Number of participants\nper 100,000 population",
    col = "State"
  )

ndis_trends_data |> 
  # filter(agegrp == "all") |> 
  filter(!is.na(count), !is.na(pop)) |> 
  group_by(agegrp, ReportDte) |> 
  summarize(count = sum(count), pop = sum(pop)) |> 
  ungroup() |> 
  mutate(
    rate_per_100k = count / (pop / 100000)
  ) |> 
  ggplot(aes(ReportDte, rate_per_100k, col = agegrp)) +
  geom_point() +
  geom_line() + 
  scale_x_date(date_labels = "%b %Y", breaks = unique(ndis_trends_data$ReportDte)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Quarter",
    y = "Number of participants\nper 100,000 population",
    col = "Age group"
  )


### testing
cg_tbl <- strayr::read_correspondence_tbl("sa2", 2016, "sa2", "2021.csv")

problematic_2021_sa2s <- c(101041022, 103031075, 107011133, 107021135, 108031161, 111031230, 
                           114011275, 116031318, 117011320, 117011324, 117011325, 118011342, 
                           123021439, 124011451, 124021456, 125011475, 125031486, 125031487, 
                           127011592, 127021521, 128021537, 197979799, 199999499, 204011061, 
                           205021080, 205021083, 205031088, 205031092, 205051099, 206041120, 
                           206041127, 208031184, 208031192, 210011227, 210051248, 297979799, 
                           299999499, 301031014, 302031036, 302031037, 304041099, 304041102, 
                           306021150, 306031162, 308031221, 308051532, 308051537, 310021279, 
                           310041298, 310041301, 311031315, 312021345, 312021347, 312031360, 
                           315011397, 319021510, 397979799, 399999499, 402041039, 402041042, 
                           403041081, 403041082, 404021098, 404021103, 404031104, 406011135, 
                           406011137, 497979799, 499999499, 501021011, 503021037, 504021052, 
                           504031055, 504031063, 504031064, 504031069, 505021087, 505021091, 
                           505031106, 505031256, 506011111, 506021120, 506021121, 506031126, 
                           506031130, 507011150, 507011151, 507011155, 507021167, 507031172, 
                           507031173, 507051189, 509011235, 511031281, 511031284, 597979799, 
                           599999499, 601041025, 602031099, 603011068, 604031093, 604031098, 
                           697979799, 699999499, 701011001, 701011003, 701021014, 701021015, 
                           701021017, 701031033, 797979799, 799999499, 801011012, 801011111, 
                           801031031, 801031032, 801031114, 801031115, 801041043, 801041045, 
                           801041116, 801041117, 801041119, 801051049, 801051123, 801051125, 
                           801051126, 801051128, 801061066, 801061068, 801061129, 801061130, 
                           801071085, 801071089, 801071132, 801081133, 801091107, 801101134, 
                           801101137, 801101145, 801101146, 801111140, 801111141, 897979799, 
                           899999499)

problematic_2016_sa2s <- cg_tbl |> 
  filter(SA2_CODE_2021 %in% as.character(problematic_2021_sa2s)) |> 
  pull(SA2_MAINCODE_2016)

# no SA2s with the codes required
readRDS("data/NDIS/psyc-soc-participants-by-sa2.rds") |> 
  filter(SA2Cd2016 %in% as.character(problematic_2016_sa2s))
# no SA2s with the codes required
readRDS(file.path(here::here(), "data", "NDIS", "participants-by-sa2.rds"))|> 
  filter(SA2Cd2016 %in% as.character(problematic_2016_sa2s))


read.csv(
  file.path(
    here::here(), 
    "source", 
    "ndis-downloads",
    "PB Participants by SA2 data MAR23.csv"
  )
) |> 
  filter(SA2Cd2016 %in% as.character(problematic_2016_sa2s))


sa2_pop |> filter(SA2 %in% problematic_2021_sa2s)
