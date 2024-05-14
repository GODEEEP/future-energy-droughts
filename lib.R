# Function library
# Cameron Bracken, April 2024
# cameron.bracken@pnnl.gov

#' Title
#'
#' @param time
#' @param unique_id
#' @param pad
#' @param run_diff
#'
#' @return
#' @export
#'
#' @examples
run_length <- function(time, unique_id = FALSE, pad = 3, run_diff = 1) {
  lt <- length(time)
  if (unique_id) {
    l <- character(lt)
    run_length_count <- list()
  } else {
    l <- numeric(lt)
  }

  i <- 1
  rl <- 1
  start_run <- 1
  end_run <- 1
  run_len <- 1

  while (i <= lt) {
    i <- i + 1
    d <- as.integer(difftime(time[i], time[i - 1], units = "hours"))
    # message(time[i], ' ', d)
    if (d == run_diff & i < lt) {
      # still in a run
      end_run <- i
      run_len <- run_len + 1
    } else {
      # run ended, or we hit the end
      if (unique_id) {
        rlc <- as.character(run_len)
        if (is.null(run_length_count[[rlc]])) {
          run_length_count[[rlc]] <- 1
        } else {
          run_length_count[[rlc]] <- run_length_count[[rlc]] + 1
        }
        l[start_run:end_run] <- sprintf(paste0("%02d_%0", pad, "d"), run_len, run_length_count[[rlc]])
      } else {
        l[start_run:end_run] <- run_len
      }
      run_len <- 1
      start_run <- i
      end_run <- i
      run_len <- 1
    }
  }
  l
}

#' Title
#'
#' @param hours
#' @param n
#'
#' @return
#' @export
#'
#' @examples
nhour_periods <- function(hours, n) {
  # generate unique periods for aggregation based on a
  # sequence of hours from hourly data. n is the number of hours
  # in each aggregation period. The data is assumed to start at the
  # beginning of a day.
  nh <- length(hours)
  np <- floor(nh / n)
  periods <- rep(1:np, each = n)
  # in case the periods dont divide the input evenly
  if (nh %% n != 0) periods <- c(periods, rep(np + 1, nh %% n))
  periods
}


#' Title
#'
#' @param names
#'
#' @return
#' @export
#'
#' @examples
dedup_names <- function(names) {
  # stolen from an old version of pandas
  # https://stackoverflow.com/questions/24685012/
  # pandas-dataframe-renaming-multiple-identically-named-columns

  counts <- list()

  for (i in 1:length(names)) {
    col <- names[i]
    cur_count <- counts[[col]]

    if (is.null(cur_count)) cur_count <- 0

    if (cur_count > 0) {
      names[i] <- sprintf("%s_%d", col, cur_count)
    }

    counts[[col]] <- cur_count + 1
  }

  return(names)
}


#' Starndardized Drought Energy Index
#'
#' @param x vector of values to compute the index for, they need not be continuous in time
#'
#' @return index value for each data point
#' @export
#'
#' @examples
sdei <- function(x) {
  n <- length(x)
  e <- ecdf(x)
  p <- (1 + e(x) * n) / (n + 2)
  if (all(x == 0)) {
    # hack for energy droughts, if all values are zero then the plotting position
    # returns a value slightly less than 1. This breaks the continuity of the energy
    # droughts over night. So in this case set the index to a large negative value
    # to allow the drought to continue over night.
    rep(-5, n)
  } else {
    qnorm(p, 0, 1)
  }
}


read_csvs_to_list <- function(dir, pattern, ...) {
  fns <- list.files(dir, pattern, full.names = TRUE, ...)
  data_list <- list()
  for (fn in fns) data_list[[fn]] <- read_csv(fn, show = FALSE, progress = FALSE)
  data_list
}

fill_leap_years <- function(data_list) {
  years <- names(data_list) |>
    basename() |>
    tools::file_path_sans_ext() |>
    strsplit("_") |>
    sapply("[", 4) |>
    as.numeric()
  for (y in years) {
    yeari <- which(years == y)
    if (leap_year(y)) {
      last_day <- data_list[[yeari]] |>
        tail(24) |>
        mutate(datetime = datetime + hours(24))
      data_list[[yeari]] <- bind_rows(data_list[[yeari]], last_day)
    }
  }
  data_list
}

fill_leap_year <- function(x, y) {
  last_day <- x |>
    tail(24) |>
    mutate(datetime = datetime + hours(24))
  bind_rows(x, last_day)
}

read_year_csv <- function(year, dir, pattern, ...) {
  fn <- list.files(dir, pattern, full.names = TRUE, ...) %>% grep(paste0("_", year), ., value = T)
  data <- read_csv(fn, show = FALSE, progress = FALSE)
  data
}

read_load_year <- function(year) {
  read_year_csv(year, c("data/tell/historic", "data/tell/rcp85hotter_ssp3"), "TELL_Bal*", recursive = TRUE) |>
    rename(
      datetime_utc = Time_UTC,
      ba = BA_Code,
      load_mwh = Scaled_TELL_BA_Load_MWh
    ) |>
    group_by(ba) |>
    mutate(
      max_load = max(load_mwh),
      load_cf = load_mwh / max_load
    ) |>
    select(datetime_utc, ba, load_cf, max_load)
}

read_wind_solar_year <- function(year, dir, pattern) {
  # browser()
  read_year_csv(year, dir, pattern) |>
    fill_leap_year(year) |>
    # convert to hour beginning
    rename(datetime_utc = datetime) |>
    mutate(datetime_utc = datetime_utc - hours(1))
}

read_experiment_scenario_year <- function(year, tech, experiment, scenario, data_dir) {
  read_wind_solar_year(year, sprintf("%s/%s/%s", data_dir, experiment, scenario), sprintf("%s*", tech))
}

read_baseline_year <- function(year, tech, experiment, data_dir) {
  read_wind_solar_year(year, sprintf("%s/%s", data_dir, experiment), sprintf("%s*", tech))
}

map_cerf_to_ba_by_county <- function(lon, lat) {
  counties <- read_sf("data/cb_2018_us_county_20m")
  county_ba <- read_csv("data/ba_service_territory_2020.csv", show = F, prog = F)

  counties_pl <- st_transform(counties, 2163)
  dsf <- st_transform(st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326), 2163)
  int <- st_intersects(dsf, counties_pl)
  ind <- sapply(int, function(x) {
    ifelse(length(x) > 0, x, NA)
  })
  fips <- counties$GEOID[ind] |> as.numeric()

  # for any points that dont fall in a county for whatever reason,
  # assign the closest
  for (i in seq_len(length(fips))) {
    if (is.na(fips[i])) {
      # print(i)
      closest_county <- counties_pl[which.min(st_distance(counties_pl, dsf[i, ])), ]
      fips[i] <- closest_county$GEOID |> as.numeric()
    }
  }

  # this randomly assigns a point to a BA in counties that have multiple BAs
  # not the best approach but its a short term fix
  tibble(County_FIPS = fips) |>
    inner_join(tibble(County_FIPS = fips) |>
      left_join(county_ba, by = "County_FIPS", relationship = "many-to-many") %>%
      mutate(r = runif(nrow(.))) |>
      arrange(r) |>
      select(-r) |>
      distinct(County_FIPS, .keep_all = TRUE), by = "County_FIPS") |>
    pull(BA_Code)

  ## TODO Missing county from tell mapping 41069, Wheeler County Oregon
}


# pnearest <- ggplot(wind_config_bau_nearest) +
#   geom_point(aes(lon, lat, color = ba)) +
#   geom_point(aes(lon, lat), size = .5, data = wind_config_eia) +
#   coord_cartesian(xlim = c(-125, -103), ylim = c(30, 50)) +
#   geom_text(aes(lon, lat, label = ba), size = 2) +
#   theme_bw()
# pcounty <- ggplot(wind_config_bau_county) +
#   geom_point(aes(lon, lat, color = ba)) +
#   geom_point(aes(lon, lat), size = .5, data = wind_config_eia) +
#   coord_cartesian(xlim = c(-125, -103), ylim = c(30, 50)) +
#   geom_text(aes(lon, lat, label = ba), size = 2) +
#   theme_bw()
# ggarrange(pcounty, pnearest, nrow = 1)
map_cerf_to_ba_by_nearest_plant <- function(lon, lat, config) {
  # assign the BA of the nearest existing wind plant
  dist <- rdist.earth(config |> select(lat, lon) |> cbind(), cbind(lat, lon))
  config$ba[dist |> apply(2, which.min)]
}

read_year_and_agg_to_ba <- function(year, scenario, iteration, data_dir) {
  # message(year)

  # years later than 2019 are future years
  cerf_dir <- ifelse(year > 2019, "cerf-future-2050", "cerf-historical-2050")
  baseline_dir <- ifelse(year > 2019, "baseline-future", "baseline-historical")

  wind_cerf <- read_experiment_scenario_year(year, "wind", cerf_dir, scenario, data_dir)
  solar_cerf <- read_experiment_scenario_year(year, "solar", cerf_dir, scenario, data_dir)

  # get the year the plant was sited using the plant id
  cerf_wind_years <- names(wind_cerf)[-1] %>%
    substr(1, ifelse(nchar(.) <= 9, 2, 4)) %>%
    as.numeric() %>%
    ifelse(. < 100, . + 2000, .)
  cerf_solar_years <- names(solar_cerf)[-1] %>%
    substr(1, ifelse(nchar(.) <= 9, 2, 4)) %>%
    as.numeric() %>%
    ifelse(. < 100, . + 2000, .)

  wind_baseline <- read_baseline_year(year, "wind", baseline_dir, data_dir)
  solar_baseline <- read_baseline_year(year, "solar", baseline_dir, data_dir)

  # handle a special case due to GCAM retiring all the eia plants in 2045
  if (iteration == 2020) {
    # 2020 only uses EIA plants
    wind <- wind_baseline
    solar <- solar_baseline
  } else if (iteration < 2045) {
    # filter out the later cited plants for any year before 2045
    wind <- wind_baseline |> inner_join(wind_cerf[, c(T, cerf_wind_years <= iteration)], by = "datetime_utc")
    solar <- solar_baseline |> inner_join(solar_cerf[, c(T, cerf_solar_years <= iteration)], by = "datetime_utc")
  } else {
    # in 2045 all the eia plants get retired and cerf will cite new plants in those locations
    wind <- wind_cerf[, c(T, cerf_wind_years <= iteration)]
    solar <- solar_cerf[, c(T, cerf_solar_years <= iteration)]
  }

  # combine eia and cerf configs
  solar_config <- solar_config_eia |>
    rename(plant_code_eia = plant_code, plant_code = plant_code_unique) %>%
    {
      if (scenario == "bau") {
        bind_rows(., solar_config_bau |> mutate(plant_code = as.character(plant_code)))
      } else {
        bind_rows(., solar_config_nz |> mutate(plant_code = as.character(plant_code)))
      }
    }
  wind_config <- wind_config_eia |>
    rename(plant_code_eia = plant_code, plant_code = plant_code_unique) %>%
    {
      if (scenario == "bau") {
        bind_rows(., wind_config_bau |> mutate(plant_code = as.character(plant_code)))
      } else {
        bind_rows(., wind_config_nz |> mutate(plant_code = as.character(plant_code)))
      }
    }

  # aggregate to BA level
  wind_ba <- wind |>
    pivot_longer(-datetime_utc, names_to = "plant_code") |>
    inner_join(wind_config, by = "plant_code") |>
    # capacity is in kw
    mutate(system_capacity = system_capacity / 1000) |>
    mutate(gen_mw = system_capacity * value) |>
    group_by(ba, datetime_utc) |>
    summarise(
      wind_gen_mw = sum(gen_mw),
      wind_gen_mwh = sum(gen_mw),
      wind_capacity = sum(system_capacity),
      n_wind_plants = n(), .groups = "drop"
    ) |>
    mutate(wind_cf = wind_gen_mw / wind_capacity)
  solar_ba <- solar |>
    pivot_longer(-datetime_utc, names_to = "plant_code") |>
    inner_join(solar_config, by = "plant_code") |>
    # capacity is in kw
    mutate(system_capacity = system_capacity / 1000) |>
    mutate(gen_mw = system_capacity * value) |>
    group_by(ba, datetime_utc) |>
    summarise(
      solar_gen_mw = sum(gen_mw),
      solar_gen_mwh = sum(gen_mw),
      solar_capacity = sum(system_capacity),
      n_solar_plants = n(), .groups = "drop"
    ) |>
    mutate(solar_cf = solar_gen_mw / solar_capacity)

  # read load data
  load_ba <- read_load_year(year) |>
    mutate(
      load_max_mwh = max_load,
      load_mwh = load_cf * max_load
    )

  # merge wind, solar and load
  solar_wind_load <-
    solar_ba |>
    inner_join(wind_ba, by = c("datetime_utc", "ba")) |>
    inner_join(load_ba, by = c("datetime_utc", "ba")) |>
    mutate(
      scenario = scenario,
      infra_year = iteration
    )

  return(solar_wind_load)
}

energy_drought <- function(gen, criteria) {
  gen |>
    # use night hours as a wild card for solar, let droughts continue overnight
    mutate(solar = ifelse(zero_prob > .99, 0, solar_s)) |>
    # filter(wind < lt & solar <= lt) |>
    filter({{ criteria }}) |>
    group_by(ba) |>
    mutate(
      run_length = run_length(datetime_utc, run_diff = periodi),
      run_id = run_length(datetime_utc, unique_id = TRUE, run_diff = periodi),
      run_length_days = run_length * periodi / 24,
      duration_days = run_length_days,
      severity_mwh = ((solar_q10 - solar_gen_mwh) + (wind_q10 - wind_gen_mwh)),
      severity_load = load_norm - load_q90,
      severity_load_mwh = residual_load_mwh - residual_load_q90
      # standardize
      # severity = (severity_mwh)/sd(severity_mwh),
    )
}

energy_drought_filter <- function(ed) {
  ed |>
    group_by(ba, run_id) |>
    # change the wild card values so they dont contribute to the magnitude
    # mutate(solar_s = ifelse(solar_s == -5, -1.28, solar_s)) |>
    summarise(
      datetime_local = datetime_local[1],
      timezone = timezone[1],
      run_length = run_length[1],
      run_length_days = run_length_days[1],
      # standardized severity metric
      severity_ws = (mean(abs(solar_s[solar_s > -5])) + mean(abs(wind_s))) / 2,
      severity_lws = (mean(abs(solar_s[solar_s > -5])) + mean(abs(wind_s)) + mean(abs(load_s))) / 3,
      # add the severity or mw for each timestep over the drought
      severity_mwh = sum(severity_mwh),
      severity_load_mwh = sum(severity_load_mwh),
      severity_load = sum(severity_load),
      load_mwh = sum(load_mwh),
      residual_load_mwh = sum(residual_load_mwh),
      wind_gen_mwh = sum(wind_gen_mwh),
      solar_gen_mwh = sum(solar_gen_mwh),
      zero_prob = mean(zero_prob),
      year = year[1],
      month = month[1],
      hour = hour[1],
      # average capacity factor over the drought duration
      wind_cf = mean(wind_cf),
      solar_cf = mean(solar_cf),
      solar_s = mean(solar_s),
      wind_s = mean(wind_s),
      load_s = mean(load_s),
      .groups = "drop"
    ) |>
    # filter single timestep events that only occur at night
    filter(!(run_length == 1 & zero_prob == 1)) |>
    filter(!is.na(severity_ws) & !is.na(severity_lws))
}

colorblind_ramp <- function(n) {
  colorRampPalette(colorblind_pal()(8)[-1])(n)
}

infra_year <- function(x) {
  x |>
    pull(plant_code) %>%
    substr(1, ifelse(nchar(.) <= 9, 2, 4)) %>%
    as.numeric() %>%
    ifelse(. < 100, . + 2000, .)
}
