# Process future energy drought data
# Cameron Bracken, April 2024
# cameron.bracken@pnnl.gov

library(tidyverse)
library(sf)
import::from(fields, rdist.earth)

source("lib.R")

future_data_dir <- "/Volumes/data/future-wind-solar/"
historical_data_dir <- "/Volumes/data/tgw-gen-historical/"

solar_config_eia <- "%s/solar/eia_solar_configs.csv" |>
  sprintf(historical_data_dir) |>
  read_csv(show = F, prog = F)
wind_config_eia <- "%s/wind/eia_wind_configs.csv" |>
  sprintf(historical_data_dir) |>
  read_csv(show = F, prog = F)
solar_config_bau <- "%s/cerf-config/solar_config_business_as_usual_ira_ccs_climate_2050.csv" |>
  sprintf(future_data_dir) |>
  read_csv(show = F, prog = F) |>
  mutate(ba = map_cerf_to_ba_by_nearest_plant(lon, lat, solar_config_eia)) # %>%
# mutate(infra_year = infra_year(.))
wind_config_bau <- "%s/cerf-config/wind_config_business_as_usual_ira_ccs_climate_2050.csv" |>
  sprintf(future_data_dir) |>
  read_csv(show = F, prog = F) |>
  mutate(ba = map_cerf_to_ba_by_nearest_plant(lon, lat, wind_config_eia)) # %>%
# mutate(infra_year = infra_year(.))
solar_config_nz <- "%s/cerf-config/solar_config_net_zero_ira_ccs_climate_2050.csv" |>
  sprintf(future_data_dir) |>
  read_csv(show = F, prog = F) |>
  mutate(ba = map_cerf_to_ba_by_nearest_plant(lon, lat, solar_config_eia)) # %>%
# mutate(infra_year = infra_year(.))
wind_config_nz <- "%s/cerf-config/wind_config_net_zero_ira_ccs_climate_2050.csv" |>
  sprintf(future_data_dir) |>
  read_csv(show = F, prog = F) |>
  mutate(ba = map_cerf_to_ba_by_nearest_plant(lon, lat, wind_config_eia)) # %>%
# mutate(infra_year = infra_year(.))

start_year_hist <- 1980
start_year_future <- 2020
# convert everything in the WECC to the same timezone for conveneince
timezone <- "US/Pacific"

# Infrastructure experiments - future infrastructure run through historical climate
message("Historical")
for (infra_year in seq(2020, 2050, by = 5)) {
  #
  message("\t", infra_year)

  for (scenario in c("nz", "bau")) {
    message("\t\t", scenario)

    1980:2019 |>
      map(read_year_and_agg_to_ba, scenario, infra_year, future_data_dir, .progress = TRUE) |>
      bind_rows() |>
      write_csv(sprintf("data/ba-aggregated/ba_hist_%s_%s_hourly.csv", infra_year, scenario))
  }
}

# Climate experiments - future infrastructure run through all future climate years
message("Future")
for (infra_year in seq(2020, 2050, by = 5)) {
  #
  message("\t", infra_year)

  for (scenario in c("nz", "bau")) {
    message("\t\t", scenario)

    2020:2059 |>
      map(read_year_and_agg_to_ba, scenario, infra_year, future_data_dir, .progress = TRUE) |>
      bind_rows() |>
      write_csv(sprintf("data/ba-aggregated/ba_future_%s_%s_hourly.csv", infra_year, scenario))
  }
}

# most expected future - future infrastructure with only the 5 associated climate years
message("Expected")
for (scenario in c("nz", "bau")) {
  #
  message("\t", scenario)

  x <- bind_rows(
    map(2020:2024, read_year_and_agg_to_ba, scenario, 2020, future_data_dir, .progress = TRUE) |> bind_rows(),
    map(2025:2029, read_year_and_agg_to_ba, scenario, 2025, future_data_dir, .progress = TRUE) |> bind_rows(),
    map(2030:2034, read_year_and_agg_to_ba, scenario, 2030, future_data_dir, .progress = TRUE) |> bind_rows(),
    map(2035:2039, read_year_and_agg_to_ba, scenario, 2035, future_data_dir, .progress = TRUE) |> bind_rows(),
    map(2040:2044, read_year_and_agg_to_ba, scenario, 2040, future_data_dir, .progress = TRUE) |> bind_rows(),
    map(2045:2049, read_year_and_agg_to_ba, scenario, 2045, future_data_dir, .progress = TRUE) |> bind_rows()
  )
  # some BAs dont start in 2025, so remove those
  # ba_starting_2025 <- x |>
  #   group_by(ba) |>
  #   summarise(start_year = min(year(datetime_utc))) |>
  #   filter(start_year == 2025) |>
  #   pull(ba)
  x |>
    # filter(ba %in% ba_starting_2025) |>
    write_csv(sprintf("data/ba-aggregated/ba_expected_future_%s_hourly.csv", scenario))
}


# create daily data from hourly
for (type in c("hist", "future", "expected_future")) {
  for (scenario in c("nz", "bau")) {
    #
    for (infra_year in seq(2020, 2050, by = 5)) {
      #
      if (type == "expected_future") {
        #
        if (infra_year == 2025) {
          csv_fn <- sprintf("data/ba-aggregated/ba_%s_%s_hourly.csv", type, scenario)
        } else {
          next
        }
      } else {
        csv_fn <- sprintf("data/ba-aggregated/ba_%s_%s_%s_hourly.csv", type, infra_year, scenario)
      }

      message(csv_fn)
      start_year <- ifelse(type == "hist", start_year_hist, start_year_future)

      read_csv(csv_fn, show = F) |>
        group_by(ba) |>
        # make everything in the west pacific time, this roughtly captures the day night cycle
        mutate(
          datetime_local = with_tz(datetime_utc, "US/Pacific"),
          year = year(datetime_utc)
        ) |>
        # due to utc to local conversion, some hour occur before the beginning of the
        # first year, chop off this period to make the periods line up nicely with
        # local time; the downside is that the last year will have a few missing
        # hours at the end of the timeseries
        filter(datetime_local >= force_tzs(ymd_hms(sprintf("%s-01-01 00:00:00", start_year)), "US/Pacific")) |>
        group_by(ba, year) |>
        # TODO correct for DST
        mutate(period = nhour_periods(hour(datetime_local), 24)) |>
        group_by(ba, year, period) |>
        summarise(
          solar_gen_mwh = sum(solar_gen_mw),
          solar_capacity_mwh = sum(solar_capacity),
          wind_gen_mwh = sum(wind_gen_mw),
          wind_capacity_mwh = sum(wind_capacity),
          n_wind_plants = n_wind_plants[1],
          n_solar_plants = n_solar_plants[1],
          # rename for the output since write_csv automatically
          # converts to utc
          datetime_utc = datetime_local[1],
          # timezone = timezone[1],
          .groups = "drop"
        ) |>
        mutate(
          wind_cf = wind_gen_mwh / wind_capacity_mwh,
          solar_cf = solar_gen_mwh / solar_capacity_mwh,
        ) -> daily
      write_csv(daily, gsub("hourly", "daily", csv_fn))
    }
  }
}
