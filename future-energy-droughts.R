# Identify future energy droughts
# Cameron Bracken, April 2024
# cameron.bracken@pnnl.gov

library(tidyverse)
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

source("lib.R")

timezone <- "US/Pacific"

drought_path <- "data/droughts/"

periods <- c(
  "daily" = 24
)
lower_thresh <- c(
  "daily" = 0.1
)
upper_thresh <- 1 - lower_thresh

i <- 1
#
period <- periodi <- periods[i]
period_name <- names(periods)[i]
lt <- lower_thresh[i]
ut <- upper_thresh[i]

hist_q10 <- read_csv("data/ba-aggregated/ba_hist_2020_bau_daily.csv") |>
  group_by(ba) |>
  summarise(
    solar_q10 = quantile(solar_gen_mwh, lt),
    wind_q10 = quantile(wind_gen_mwh, lt)
  )

period_fns <- list.files("data/ba-aggregated", sprintf("*_%s.csv", period_name), full.names = TRUE)

for (fn in period_fns) {
  #
  message("\t", fn)

  message("\t\tReading data")
  data_type <- ifelse(str_detect(fn, "ba_future"), "future",
    ifelse(str_detect(fn, "ba_hist"), "hist", "expected_future")
  )
  start_year <- ifelse(data_type == "hist", 1981, 2025)

  ba_gen_all <- read_csv(fn) %>%
    mutate(
      datetime_local = with_tz(datetime_utc, "US/Pacific"),
      year = year(datetime_local),
      month = month(datetime_local),
      day = day(datetime_local),
      jday = yday(datetime_local),
      hour = hour(datetime_local),
      week = week(datetime_local)
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
    group_by(ba, year) |>
    group_by(ba) |>
    mutate(
      # standardized values
      solar_s = sdei(solar_gen_mwh),
      wind_s = sdei(wind_gen_mwh),
      # quantiles
      solar_q10 = quantile(solar_gen_mwh, lt),
      wind_q10 = quantile(wind_gen_mwh, lt),
      zero_prob = 0
    ) |>
    ungroup() #|>
  # left_join(hist_q10, by = "ba")

  n_years <- ba_gen_all$year |>
    unique() |>
    length()


  ####################################
  ####################################
  ####################################
  # Droughts
  ####################################
  ####################################
  ####################################
  # message("Droughts")

  message("\t\tEnergy droughts")
  for (bai in unique(ba_gen_all$ba)) {
    #
    message("\t\t\t", bai)

    ba_gen <- ba_gen_all |> filter(ba == bai)
    # wind and solar
    # -1.28 corresponds to 10th percentile
    ws_droughts_all <- ba_gen |> energy_drought(wind_s < -1.28 & solar_s < -1.28)
    # ws_droughts_all <- ba_gen |> energy_drought(wind_gen_mwh < wind_q10 & solar_gen_mwh < solar_q10)
    # droughts_all = ba_gen |> energy_drought(wind_gen_mwh < wind_q10 & solar_gen_mwh <= solar_q10)
    # droughts_all = ba_gen |> energy_drought(wind < lt & solar <= lt)
    ws_droughts <- energy_drought_filter(ws_droughts_all)


    # wind
    # -1.28 corresponds to 10th percentile
    wind_droughts_all <- ba_gen |> energy_drought(wind_s < -1.28)
    # wind_droughts_all <- ba_gen |> energy_drought(wind_gen_mwh < wind_q10)
    wind_droughts <- energy_drought_filter(wind_droughts_all)

    # solar
    # -1.28 corresponds to 10th percentile
    solar_droughts_all <- ba_gen |> energy_drought(solar_s < -1.28)
    # solar_droughts_all <- ba_gen |> energy_drought(solar_gen_mwh < solar_q10)
    solar_droughts <- energy_drought_filter(solar_droughts_all)


    # dont allow single hour droughts
    if (period_name == "hourly") {
      ws_droughts <- ws_droughts |> filter(run_length > 1)
      wind_droughts <- wind_droughts |> filter(run_length > 1)
      solar_droughts <- solar_droughts |> filter(run_length > 1)
    }

    file_suffix <- fn |>
      basename() |>
      tools::file_path_sans_ext()

    # write_csv converts the local time to UTC when it writes out
    write_ba_drought <- function(x, drought_type, period_name, file_suffix, bai) {
      if (nrow(x) > 1) {
        x |>
          rename(datetime_utc = datetime_local) |>
          write_csv(sprintf(
            "%s/%s_droughts_%s_%s.csv", drought_path, drought_type, file_suffix, bai
          ), progress = F)
      }
    }
    ws_droughts |> write_ba_drought("ws", period_name, file_suffix, bai)
    wind_droughts |> write_ba_drought("wind", period_name, file_suffix, bai)
    solar_droughts |> write_ba_drought("solar", period_name, file_suffix, bai)
  }

  # read the BA files and combine
  read_combine_ba_droughts <- function(drought_type, file_suffix) {
    combo_fn <- sprintf("%s/%s_droughts_%s.csv", drought_path, drought_type, file_suffix)
    ba_fns <- drought_path |>
      list.files(sprintf("^%s_droughts_%s_*", drought_type, file_suffix), full.names = TRUE) %>%
      grep(combo_fn, ., value = TRUE, invert = TRUE)
    ba_fns |>
      map(function(x) {
        read_csv(x, show = F, progress = F)
      }) |>
      bind_rows() |>
      write_csv(combo_fn, progress = F)
    unlink(ba_fns)
  }

  read_combine_ba_droughts("ws", file_suffix)
  read_combine_ba_droughts("wind", file_suffix)
  read_combine_ba_droughts("solar", file_suffix)
}
