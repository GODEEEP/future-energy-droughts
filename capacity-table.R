# Capacity table for the paper
# Cameron Bracken, April 2024
# cameron.bracken@pnnl.gov

library(tidyverse)
library(xtable)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

source("lib.R")

timezone <- "US/Pacific"

drought_path <- "data/droughts/"

periods <- c(
  # "hourly" = 1,
  "daily" = 24
)
# periods = c('1_1-hour'=1)
lower_thresh <- c(
  # "hourly" = 0.1,
  "daily" = 0.1
)
upper_thresh <- 1 - lower_thresh

bas_to_include <- c(
  "PSEI", "AVA", "PGE", "BPAT", # NW
  "PACW", "IPCO", "NWMT",
  "PACE", "WACM", "PSCO", "PNM",
  "CISO", "LDWP", "NEVP", "WALC", "AZPS", "SRP", "TEPC",
  "BANC", "CHPD", "DEAA", "DOPD",
  # "EPE",
  "GCPD", "GRID", "GRIF", "GRMA", "GWA", "HGMA", "IID",
  "SCL", "TIDC", "TPWR",
  "WAUW", "WWA"
)

i <- 1

period <- periodi <- periods[i]
period_name <- names(periods)[i]
lt <- lower_thresh[i]
ut <- upper_thresh[i]
message("\n", period_name)

period_fns <- list.files("data/ba-aggregated", sprintf("*_%s.csv", period_name), full.names = TRUE) # %>%
period_fns <- period_fns[c(3, 4, 9, 10, 15, 16)]
infra_years <- period_fns |>
  strsplit("_") |>
  sapply("[", 3)
scenarios <- period_fns |>
  strsplit("_") |>
  sapply("[", 4)

# grep("2045|2050", ., value = T)
cap_pct_load_list <- list()
for (j in 1:length(period_fns)) {
  #
  fn <- period_fns[j]
  infra_year <- infra_years[j]
  scenario <- scenarios[j]
  message("\t", fn)

  message("\t\tReading data")
  data_type <- ifelse(str_detect(fn, "ba_future"), "future",
    ifelse(str_detect(fn, "ba_hist"), "hist", "expected_future")
  )
  start_year <- ifelse(data_type == "hist", 1981, 2025)

  ba_gen_all <- read_csv(fn, show = FALSE, progress = FALSE) %>%
    mutate(
      datetime_local = with_tz(datetime_utc, "US/Pacific"),
      year = year(datetime_local),
      month = month(datetime_local),
      day = day(datetime_local),
      jday = yday(datetime_local),
      hour = hour(datetime_local),
      week = week(datetime_local),
      # load_mwh = load_cf * max_load,
      residual_load_mwh = load_mwh - solar_gen_mwh - wind_gen_mwh
    ) |>
    # something is wrong with the 1980 data
    filter(year >= start_year) %>%
    {
      # hack for the expected future case, some BAs only appear in later
      # infrastructure years so remove them
      if (data_type == "expected_future") {
        group_by(., ba) |>
          mutate(first_year = min(year)) |>
          filter(first_year == start_year) |>
          select(-first_year) |>
          ungroup()
      } else {
        .
      }
    } |>
    # normalize the laod data each year to account for increasing load signal
    group_by(ba, year) |>
    mutate(
      load_norm = (residual_load_mwh - mean(residual_load_mwh)) / sd(residual_load_mwh),
      load_max_norm = (load_max_mwh - mean(load_mwh)) / sd(load_mwh)
    ) |>
    group_by(ba, week, hour) |>
    # group_by(ba, hour) |>
    mutate(
      zero_prob = length(which(solar_gen_mwh == 0)) / length(solar_gen_mwh),
      # solar_s = sdei(solar_gen_mwh),
      # wind_s = sdei(wind_gen_mwh),
      # load_s = sdei(load_norm),
      # solar_q = ecdf(solar_gen_mwh)(solar_gen_mwh),
      # wind_q = ecdf(wind_gen_mwh)(wind_gen_mwh),
      # load_q = ecdf(load_norm)(load_norm),
      # solar = ecdf(solar_gen_mwh)(solar_gen_mwh),
      # wind = ecdf(wind_gen_mwh)(wind_gen_mwh),
      # load = ecdf(load_norm)(load_norm),
      # residual_load = ecdf(residual_load_mwh)(residual_load_mwh),
      # solar_q10 = quantile(solar_gen_mwh, lt),
      # wind_q10 = quantile(wind_gen_mwh, lt),
      # load_q90 = quantile(load_norm, ut),
      # residual_load_q90 = quantile(residual_load_mwh, ut)
    ) |>
    group_by(ba, hour) |>
    mutate(
      # standardized values
      solar_s = sdei(solar_gen_mwh),
      wind_s = sdei(wind_gen_mwh),
      load_s = sdei(load_norm),
      # quantiles
      solar_q10 = quantile(solar_gen_mwh, lt),
      wind_q10 = quantile(wind_gen_mwh, lt),
      load_q90 = quantile(load_norm, ut),
      residual_load_q90 = quantile(residual_load_mwh, ut)
    ) |>
    ungroup()

  cap_pct_load_list[[j]] <- ba_gen_all |>
    group_by(ba) |>
    summarise(
      solar_cap_pct_load = solar_capacity_mwh[1] / load_max_mwh[1] * 100,
      wind_cap_pct_load = wind_capacity_mwh[1] / load_max_mwh[1] * 100,
      solar_cap_gw = solar_capacity_mwh[1] / 24 / 1000,
      wind_cap_gw = wind_capacity_mwh[1] / 24 / 1000,
    ) |>
    mutate(
      solar_cap_pct_load = round(solar_cap_pct_load, 1),
      wind_cap_pct_load = round(wind_cap_pct_load, 1),
      solar_cap_gw = round(solar_cap_gw, 1),
      wind_cap_gw = round(wind_cap_gw, 1),
      infra_year = infra_year, scenario = scenario
    )
}

capacity_table <- cap_pct_load_list |>
  bind_rows() |>
  pivot_wider(
    id_cols = ba,
    names_from = c(infra_year, scenario),
    values_from = c(solar_cap_gw, wind_cap_gw)
  ) |>
  filter(ba %in% bas_to_include)

capacity_table |>
  write_csv("data/capacity_table.csv")

capacity_table |>
  select(ends_with("_nz")) |>
  rename_all(~ c("s2020", "s2035", "s2050", "w2020", "w2035", "w2050")) |>
  xtable(digits = 1, ) |>
  print.xtable(include.rownames = FALSE)

capacity_table |>
  select(ends_with("_bau")) |>
  rename_all(~ c("s2020", "s2035", "s2050", "w2020", "w2035", "w2050")) |>
  xtable(digits = 1, ) |>
  print.xtable(include.rownames = FALSE)


capacity_pct_load_table <- cap_pct_load_list |>
  bind_rows() |>
  pivot_wider(
    id_cols = ba,
    names_from = c(infra_year, scenario),
    values_from = c(solar_cap_pct_load, wind_cap_pct_load)
  ) |>
  filter(ba %in% bas_to_include)

capacity_pct_load_table |>
  write_csv("data/capacity_table_pct_load.csv")
