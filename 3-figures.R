# Postprocess future energy droughts and generate plots for the paper
# Cameron Bracken, April 2024
# cameron.bracken@pnnl.gov

library(tidyverse)
library(sf)
import::from(ggthemes, scale_color_colorblind, colorblind_pal)
import::from(ggfx, with_shadow)
import::from(igraph, make_undirected_graph, gorder, largest_component)
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

plot_drought_type <- "ws_droughts" # "ws_droughts"
time_scales <- c("daily")
decarb_scenarios <- c("bau", "nz")
infra_years <- seq(2020, 2050, by = 5)
weather_years <- c("ba_future", "ba_hist")

dir.create("plots", showWarnings = F)

seasons <- c("Winter", "Spring", "Summer", "Fall")

states_sf <- st_read("/Volumes/data/shapefiles/cb_2018_us_state_5m/cb_2018_us_state_5m.shp") |>
  filter(NAME %in% c(
    "California", "Washington", "Oregon", "Idaho", "Nevada", "Montana",
    "New Mexico", "Arizona", "Utah", "Colorado", "Wyoming"
  ))

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

infra_year_colors <- colorblind_pal()(8)[-1]
infra_year_colors <- viridis::mako(8)[-1]
# infra_year_colors <- viridis::plasma(8)[-1]

# tweak the BA point locations for better plotting
ba_centroids <- read_csv("data/ba-centroids.csv", show = F, progress = F) |>
  filter(ba %in% bas_to_include) |>
  # adjust positions for plot clarity
  mutate(
    lon = case_when(
      ba == "CISO" ~ lon - 3,
      ba == "SRP" ~ lon + 1,
      ba == "AZPS" ~ lon - 2,
      ba == "PACW" ~ lon - 2,
      ba == "AVA" ~ lon,
      ba == "WALC" ~ lon + 1,
      ba == "BPAT" ~ lon + 1,
      ba == "PSEI" ~ lon - 1,
      .default = lon
    ),
    lat = case_when(
      ba == "CISO" ~ lat + 3,
      ba == "TEPC" ~ lat - 1,
      ba == "PSCO" ~ lat - 1.5,
      ba == "WALC" ~ lat + 3,
      ba == "PACW" ~ lat - 1,
      ba == "AVA" ~ lat + 1,
      ba == "PSEI" ~ lat + 1,
      .default = lat
    )
  )

# AVA  - Avista Corporation
# AZPS - Arizona Public ServiceCompany
# BANC - Balancing Authority ofNorthern California
# BPAT - Bonneville PowerAdministration-Transmission
# CHPD - PUD No. 1 of Chelan County
# CISO - California Independent System Operator
# DEAA - Arlington Valley, LLC
# DOPD - PUD No. 1 of Douglas County
# EPE  - El Paso Electric Company
# GCPD - PUD No. 2 of Grant County
# GRID - Gridforce Energy Management, LLC
# GRIF - Griffith Energy, LLC
# GRMA - Gila River Power, LP
# GWA  - NaturEner Power Watch, LLC
# HGMA - New Harquahala Generating Company, LLC
# IID  - Imperial Irrigation District
# IPCO - Idaho Power Company
# LDWP - Los Angeles Departmentof Water and Power
# NEVP - Nevada Power Company
# NWMT - NorthWestern Energy
# PACE - PacifiCorp East
# PACW - PacifiCorp West
# PGE  - Portland General Electric Company
# PNM  - Public Service Company of New Mexico
# PSCO - Public Service Company of Colorado
# PSEI - Puget Sound Energy
# SCL  - Seattle City Light
# SRP  - Salt River Project
# TEPC - Tucson Electric Power Company
# TIDC - Turlock Irrigation District
# TPWR - City of Tacoma, Department of Public Utilities
# WACM - Western Area Power Administration,Colorado-Missouri Region
# WALC - Western Area Power Administration, Lower Colorado Region
# WAUW - Wester Area Power Administration, Upper Great Plains West
# WWA  - NaturEner Wind Watch, LLC

drought_list <- list()
for (drought_type in plot_drought_type) {
  for (time_scale in time_scales) {
    for (decarb_scenario in decarb_scenarios) {
      for (infra_year in infra_years) {
        for (weather_year in weather_years) {
          #
          fn <- paste(drought_type, weather_year, infra_year, decarb_scenario, time_scale, sep = "_") %>%
            paste0("data/droughts/", ., ".csv")
          message(fn)

          drought_list[[fn]] <- fn |>
            read_csv(progress = F, show = F) |>
            mutate(
              drought_type = drought_type,
              time_scale = time_scale,
              weather_year = weather_year,
              infra_year = infra_year,
              decarb_scenario = decarb_scenario
            )
        }
      }
    }
  }
}
droughts_ <- bind_rows(drought_list) |> filter(ba %in% bas_to_include) # filter(infra_year <= 2040 & infra_year > 2020)
droughts <- droughts_ |>
  group_by(decarb_scenario, drought_type, ba) |>
  # normalize the severity to help with the plotting
  mutate(
    severity_percent_load = severity_mwh / residual_load_mwh,
    severity_mwh = severity_mwh / max(severity_mwh),
    ba = factor(ba, levels = bas_to_include)
  ) |>
  mutate(
    month = month(datetime_utc),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% 3:5 ~ "Spring",
      month %in% 6:8 ~ "Summer",
      month %in% 9:11 ~ "Fall"
    )
  )

compute_drought_stats <- function(d) {
  d |>
    group_by(ba, time_scale, infra_year, drought_type, decarb_scenario, weather_year) |>
    summarise(
      median_dur = median(run_length_days),
      mean_dur = mean(run_length_days),
      p99_dur = quantile(run_length_days, .99),
      p90_dur = quantile(run_length_days, .90),
      max_dur = max(run_length_days),
      min_dur = min(run_length_days),
      mean_sev = mean(ifelse(substr(drought_type[1], 1, 2) == "ws", severity_ws, severity_lws)),
      mean_sev_mwh = mean(severity_mwh),
      p99_sev = quantile(ifelse(substr(drought_type[1], 1, 2) == "ws", severity_ws, severity_lws), .99, na.rm = T),
      p99_sev_mwh = quantile(severity_mwh, .99),
      p90_sev = quantile(ifelse(substr(drought_type[1], 1, 2) == "ws", severity_ws, severity_lws), .90, na.rm = T),
      p90_sev_mwh = quantile(severity_mwh, .90),
      # p99_sev_pct_load = quantile(severity_mwh / load_mwh, .99),
      # p90_sev_pct_load = quantile(severity_mwh / load_mwh, .90),
      max_sev = max(ifelse(substr(drought_type[1], 1, 2) == "ws", severity_ws, severity_lws)),
      max_sev_mwh = max(severity_mwh),
      min_sev = min(ifelse(substr(drought_type[1], 1, 2) == "ws", severity_ws, severity_lws)),
      min_sev_mwh = min(severity_mwh),
      frequency = length(run_id) / length(unique(year)),
      .groups = "drop"
    ) # |>
  # pivot_longer(-c(ba, period, period_length_hours, lon, lat), names_to = "stat", values_to = "value")
}

drought_stats <- compute_drought_stats(droughts)

########################################################################
# Severity bubble chart
########################################################################
p_severity_bubble <- drought_stats |>
  filter(drought_type == plot_drought_type & decarb_scenario == "nz") |>
  group_by(ba, infra_year, weather_year) |>
  summarise(severity = quantile(p99_sev_mwh, 0.5), .groups = "drop") |>
  filter(weather_year == "ba_hist") |>
  left_join(ba_centroids, by = "ba") |>
  ggplot() +
  geom_sf(data = states_sf, fill = gray(.95)) +
  geom_point(aes(lon, lat, size = severity, fill = severity), shape = 21) +
  facet_wrap(~infra_year, nrow = 2) +
  scale_size_continuous(limits = c(0, 1), range = c(1, 12), breaks = seq(0, 1, by = .2)) +
  theme_minimal() +
  scale_fill_viridis_c(option = "B", direction = -1, guide = "legend", limits = c(0, 1), breaks = seq(0, 1, by = .2)) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0, "pt"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.88, .25),
    legend.box = "horizontal"
  ) +
  labs(size = "Normalized Energy\nDrought Severity", fill = "Normalized Energy\nDrought Severity")
# guides(size = guide_legend(reverse = TRUE))
p_severity_bubble
ggsave("plots/severity_bubble_map_nz_%s.pdf" |> sprintf(plot_drought_type), p_severity_bubble, width = 10, height = 6)

########################################################################
# Difference in severity between historical and future periods
########################################################################

severity <- droughts |>
  filter(drought_type == plot_drought_type) |>
  group_by(ba, weather_year, infra_year, decarb_scenario, year) |>
  summarise(severity = mean(severity_mwh), .groups = "drop") |>
  mutate(year = ifelse(weather_year == "ba_future", year - 40, year)) |>
  pivot_wider(
    id_cols = c(ba, infra_year, decarb_scenario, year),
    values_from = severity, names_from = weather_year
  ) |>
  mutate(
    pdiff = (ba_future - ba_hist) / ((ba_hist + ba_future) / 2) * 100,
    diff = ba_future - ba_hist
  )

severity_infra <- droughts |>
  filter(drought_type == plot_drought_type) |>
  group_by(ba, weather_year, infra_year, decarb_scenario, year) |>
  summarise(severity = mean(severity_mwh), .groups = "drop") |>
  mutate(year = ifelse(weather_year == "ba_future", year - 40, year)) |>
  pivot_wider(
    id_cols = c(ba, weather_year, decarb_scenario, year),
    values_from = severity, names_from = infra_year
  ) |>
  mutate(
    pdiff = (`2050` - `2020`) / ((`2050` + `2020`) / 2) * 100,
    diff = `2050` - `2020`
  )

plot_severity_diff <- function(x) {
  x |> ggplot() +
    geom_boxplot(aes(factor(ba), diff, fill = factor(infra_year)),
      position = position_dodge2(preserve = "single"), outlier.shape = NA, linewidth = .3
    ) +
    # facet_wrap(~decarb_scenario, nrow = 2) +
    theme_bw() +
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual("Infrastructure Year", values = infra_year_colors) +
    labs(x = "Balancing Authority", y = "Normalized Energy Drought Severity\nDifference [Future - Historical]") +
    scale_y_continuous(limits = c(-0.3, 0.4)) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

p_sev_infra_diff_nz <- severity_infra |>
  filter(decarb_scenario == "nz") |>
  plot_severity_diff()
p_sev_infra_diff_nz
ggsave("plots/severity_diff_infra_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_sev_infra_diff_nz,
  width = 10, height = 4, dpi = 600
)

p_sev_diff_nz <- severity |>
  filter(decarb_scenario == "nz") |>
  plot_severity_diff()
p_sev_diff_nz
ggsave("plots/severity_diff_hist_future_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_sev_diff_nz,
  width = 10, height = 4, dpi = 600
)

p_sev_diff_bau <- severity |>
  filter(decarb_scenario == "bau") |>
  plot_severity_diff()
p_sev_diff_bau
ggsave("plots/severity_diff_hist_future_bau_%s.pdf" |> sprintf(plot_drought_type),
  p_sev_diff_bau,
  width = 10, height = 4, dpi = 600
)

########################################################################
# Severity variability bubble chart
########################################################################
p_variability_bubble <- severity |>
  group_by(ba, infra_year, decarb_scenario) |>
  summarise(sd_diff = sd(diff, na.rm = T), diff = mean(diff, na.rm = T), .groups = "drop") |>
  left_join(ba_centroids, by = "ba") |>
  filter(decarb_scenario == "nz") |>
  ggplot() +
  geom_sf(data = states_sf, fill = gray(0.95)) +
  geom_point(aes(lon, lat, size = sd_diff, fill = sd_diff), shape = 21) +
  facet_wrap(~infra_year, nrow = 2) +
  scale_size_continuous(range = c(1, 12), limits = c(0.01, 0.2), breaks = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2)) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.9, .22),
    legend.box = "horizontal"
  ) +
  # guides(size = guide_legend(reverse = TRUE)) +
  scale_fill_viridis_c(
    option = "G", direction = -1, limits = c(0.01, 0.2), breaks = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2),
    guide = "legend"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0, "pt"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.88, .25),
    legend.box = "horizontal"
  )
p_variability_bubble
ggsave("plots/severity_diff_variability_bubble_map_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_variability_bubble,
  width = 10, height = 6
)

########################################################################
# Difference in severity between historical and future periods, seasonal
########################################################################
seasonal_severity <- droughts |>
  filter(drought_type == plot_drought_type) |>
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% 3:5 ~ "spring",
    month %in% 6:8 ~ "summer",
    month %in% 9:11 ~ "fall"
  )) |>
  group_by(ba, season, weather_year, infra_year, decarb_scenario, year) |>
  summarise(severity = mean(severity_mwh), .groups = "drop") |>
  mutate(
    year = ifelse(weather_year == "ba_future", year - 40, year),
    season = factor(season, levels = c("winter", "spring", "summer", "fall"))
  ) |>
  pivot_wider(
    id_cols = c(ba, season, infra_year, decarb_scenario, year),
    values_from = severity, names_from = weather_year
  ) |>
  mutate(diff = ba_future - ba_hist)

plot_seasonal_severity_diff <- function(x) {
  x |> ggplot() +
    geom_boxplot(aes(factor(ba), diff, fill = factor(infra_year)),
      position = position_dodge2(preserve = "single"), outlier.shape = NA, linewidth = .3
    ) +
    facet_wrap(~season) +
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual("Infrastructure Year", values = infra_year_colors) +
    labs(x = "Balancing Authority", y = "Normalized Energy Drought Severity Difference [Future - Historical]") +
    scale_y_continuous(limits = c(-0.3, 0.4)) +
    theme_bw() +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

p_seas_sev_diff_nz <- seasonal_severity |>
  filter(decarb_scenario == "nz") |>
  plot_seasonal_severity_diff()
p_seas_sev_diff_nz
ggsave("plots/severity_seasonal_diff_hist_future_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_seas_sev_diff_nz,
  width = 16, height = 7, dpi = 600
)

p_seas_sev_diff_bau <- seasonal_severity |>
  filter(decarb_scenario == "bau") |>
  plot_seasonal_severity_diff()
p_seas_sev_diff_bau
ggsave("plots/severity_seasonal_diff_hist_future_bau_%s.pdf" |> sprintf(plot_drought_type),
  p_seas_sev_diff_bau,
  width = 16, height = 7, dpi = 600
)

############################################################################
# severity/intensity/magnitude
############################################################################

plot_severity_by_ba <- function(x, wy, dtype = plot_drought_type) {
  x |>
    filter(weather_year == wy) |>
    filter(drought_type == dtype) |>
    ggplot() +
    geom_boxplot(aes(factor(ba), severity_mwh, fill = factor(infra_year)),
      position = position_dodge2(preserve = "single"), outlier.shape = NA, linewidth = .3
    ) +
    # facet_wrap(~decarb_scenario, ncol = 1) +
    theme_bw() +
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual("Infrastructure Year", values = infra_year_colors) +
    labs(x = "Balancing Authority", y = "Normalized Energy Drought Severity") +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
    )
}


p_sev_hist_nz <- droughts |>
  filter(decarb_scenario == "nz") |>
  plot_severity_by_ba("ba_hist") #+ scale_y_continuous(limits=c(0,200))
p_sev_hist_nz
ggsave("plots/severity_hist_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_sev_hist_nz,
  width = 10, height = 4
)

p_sev_future_nz <- droughts |>
  filter(decarb_scenario == "nz") |>
  plot_severity_by_ba("ba_future")
p_sev_future_nz
ggsave("plots/severity_future_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_sev_future_nz,
  width = 10, height = 4
)

p_sev_hist_bau <- droughts |>
  filter(decarb_scenario == "bau") |>
  plot_severity_by_ba("ba_hist") #+ scale_y_continuous(limits=c(0,200))
p_sev_hist_bau
ggsave("plots/severity_hist_bau_%s.pdf" |> sprintf(plot_drought_type),
  p_sev_hist_bau,
  width = 10, height = 4
)

p_sev_future_bau <- droughts |>
  filter(decarb_scenario == "bau") |>
  plot_severity_by_ba("ba_future")
p_sev_future_bau
ggsave("plots/severity_future_bau_%s.pdf" |> sprintf(plot_drought_type),
  p_sev_future_bau,
  width = 10, height = 4
)


##############################################################
# variance tests
#############################################################
severity2020 <- severity |>
  group_by(ba, decarb_scenario, infra_year) |>
  filter(infra_year == 2020)
severity |>
  group_by(ba, decarb_scenario, infra_year) |>
  filter(infra_year > 2020) |>
  group_split() |>
  map(function(x) {
    # if (nrow(x) < 6) {
    #   return(NA)
    # }
    bai <- x$ba[1]
    dsi <- x$decarb_scenario[1]
    iyi <- x$infra_year[1]
    baseline <- severity2020 |>
      filter(
        ba == bai, decarb_scenario == dsi,
      )
    if (nrow(baseline) < 1 | nrow(x) < 1) {
      pvalue <- NA
    } else {
      # small p-value indicates variances are different
      pvalue <- var.test(x$diff, baseline$diff)$p.value
    }
    # with(x, message(bai, " ", dsi, " ", wyi, " ", dti, " ", nrow(x), " ", nrow(baseline)))
    tibble(ba = bai, decarb_scenario = dsi, infra_year = iyi, p_value = pvalue)
  }) |>
  bind_rows() -> var_tests
var_test_pct_bau <- (var_tests |>
  filter(decarb_scenario == "bau") |>
  filter(p_value < 0.01) |> nrow()) / nrow(var_tests |> filter(decarb_scenario == "bau"))
var_test_pct_nz <- (var_tests |>
  filter(decarb_scenario == "nz") |>
  filter(p_value < 0.01) |> nrow()) / nrow(var_tests |> filter(decarb_scenario == "nz"))
message("BAs with variance different than baseline, bau: ", round(var_test_pct_bau * 100, 1), "%")
message("BAs with variance different than baseline, nz: ", round(var_test_pct_nz * 100, 1), "%")

##############################################################
# mean tests difference from zero
#############################################################
severity |>
  group_by(ba, decarb_scenario, infra_year) |>
  filter(infra_year > 2020) |>
  group_split() |>
  map(function(x) {
    bai <- x$ba[1]
    dsi <- x$decarb_scenario[1]
    iyi <- x$infra_year[1]
    if (nrow(x) < 1) {
      pvalue <- NA
    } else {
      # small p-values indicate mean is not zero, large values fail to reject
      # pvalue <- wilcox.test(x$severity_mwh, mu = 0)$p.value
      pvalue <- t.test(x$diff)$p.value
    }
    tibble(ba = bai, decarb_scenario = dsi, infra_year = iyi, p_value = pvalue)
  }) |>
  bind_rows() -> mean_tests_diff_zero

mean_zero_test_bau <- 1 - (mean_tests_diff_zero |>
  filter(decarb_scenario == "bau") |>
  filter(p_value < 0.01) |> nrow()) / nrow(mean_tests_diff_zero |> filter(decarb_scenario == "bau"))
mean_zero_test_nz <- 1 - (mean_tests_diff_zero |>
  filter(decarb_scenario == "nz") |>
  filter(p_value < 0.01) |> nrow()) / nrow(mean_tests_diff_zero |> filter(decarb_scenario == "nz"))
message("BAs with mean equal to zero, bau: ", round(mean_zero_test_bau * 100, 1), "%")
message("BAs with mean equal to zero, nz: ", round(mean_zero_test_nz * 100, 1), "%")

##############################################################
# mean tests diff from baseline
#############################################################
severity2020 <- severity |>
  group_by(ba, decarb_scenario, infra_year) |>
  filter(infra_year == 2020)
severity |>
  group_by(ba, decarb_scenario, infra_year) |>
  filter(infra_year > 2020) |>
  group_split() |>
  map(function(x) {
    bai <- x$ba[1]
    dsi <- x$decarb_scenario[1]
    iyi <- x$infra_year[1]
    baseline <- severity2020 |>
      filter(
        ba == bai, decarb_scenario == dsi,
      )
    if (nrow(baseline) < 1 | nrow(x) < 1) {
      pvalue <- NA
    } else {
      # pvalue <- wilcox.test(x$severity_mwh, baseline$severity_mwh, alternative = "two.sided")$p.value
      # small p-value indicated mean is different in both sammples
      pvalue <- t.test(x$diff, baseline$diff, alternative = "two.sided", var.equal = FALSE)$p.value
    }
    tibble(ba = bai, decarb_scenario = dsi, infra_year = iyi, p_value = pvalue)
  }) |>
  bind_rows() -> mean_tests_diff_from_baseline
mean_two_test_bau <- 1 - (mean_tests_diff_from_baseline |>
  filter(decarb_scenario == "bau") |>
  filter(p_value < 0.01) |> nrow()) / nrow(mean_tests_diff_from_baseline |> filter(decarb_scenario == "bau"))
mean_two_test_nz <- 1 - (mean_tests_diff_from_baseline |>
  filter(decarb_scenario == "nz") |>
  filter(p_value < 0.01) |> nrow()) / nrow(mean_tests_diff_from_baseline |> filter(decarb_scenario == "nz"))
message("BAs with mean same as baseline, bau: ", round(mean_two_test_bau * 100, 1), "%")
message("BAs with mean same as baseline, nz: ", round(mean_two_test_nz * 100, 1), "%")

##############################################################
# difference between historical and future duration
#############################################################
duration <- droughts |>
  filter(drought_type == plot_drought_type) |>
  group_by(ba, weather_year, infra_year, decarb_scenario, year) |>
  summarise(duration = mean(run_length_days), .groups = "drop") |>
  mutate(year = ifelse(weather_year == "ba_future", year - 40, year)) |>
  pivot_wider(
    id_cols = c(ba, infra_year, decarb_scenario, year),
    values_from = duration, names_from = weather_year
  ) |>
  mutate(
    pdiff = (ba_future - ba_hist) / ((ba_hist + ba_future) / 2) * 100,
    diff = ba_future - ba_hist
  )

plot_duration_diff <- function(x) {
  x |> ggplot() +
    geom_boxplot(aes(factor(ba), diff, fill = factor(infra_year)),
      position = position_dodge2(preserve = "single"), outlier.shape = NA, linewidth = .3
    ) +
    # facet_wrap(~decarb_scenario, nrow = 2) +
    theme_bw() +
    guides(fill = guide_legend(nrow = 1)) +
    scale_fill_manual("Infrastructure Year", values = infra_year_colors) +
    labs(x = "Balancing Authority", y = "Compound Energy Drought Duration\nDifference [Future - Historical]") +
    scale_y_continuous(limits = c(-0.3, 0.4)) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

p_dur_diff_nz <- duration |>
  filter(decarb_scenario == "nz") |>
  plot_duration_diff()
p_dur_diff_nz
ggsave("plots/duration_diff_hist_future_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_dur_diff_nz,
  width = 10, height = 4, dpi = 600
)

p_dur_diff_bau <- duration |>
  filter(decarb_scenario == "bau") |>
  plot_duration_diff()
p_dur_diff_bau
ggsave("plots/duration_diff_hist_future_bau_%s.pdf" |> sprintf(plot_drought_type),
  p_dur_diff_bau,
  width = 10, height = 4, dpi = 600
)


##################################################################################
# drought duration
##################################################################################
plot_duration_by_ba <- function(x, wy, dtype = plot_drought_type) {
  x |>
    filter(weather_year == wy) |>
    filter(drought_type == dtype) |>
    ggplot() +
    geom_boxplot(aes(factor(ba), run_length_days, fill = factor(infra_year)),
      position = position_dodge2(preserve = "single"), linewidth = .3
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_bw() +
    # facet_wrap(~decarb_scenario) +
    scale_fill_manual("Infrastructure Year", values = infra_year_colors) +
    scale_y_continuous(breaks = 1:15) +
    labs(x = "Balancing Authority", y = "Drought length [days]") +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

plot_duration_by_ba_bars <- function(x, wy, dtype = plot_drought_type) {
  x |>
    filter(weather_year == wy) |>
    filter(drought_type == dtype) |>
    group_by(ba, infra_year) |>
    summarise(max = max(run_length_days)) |>
    ggplot() +
    geom_bar(aes(factor(ba), max, fill = factor(infra_year)),
      position = position_dodge2(preserve = "single"), stat = "identity"
    ) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_bw() +
    # facet_wrap(~decarb_scenario) +
    scale_fill_manual("Infrastructure Year", values = infra_year_colors) +
    scale_y_continuous(breaks = 0:15) +
    labs(x = "Balancing Authority", y = "Drought length [days]") +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

p_dur_hist_nz <- droughts |>
  filter(decarb_scenario == "nz") |>
  plot_duration_by_ba_bars("ba_hist")
p_dur_hist_nz
ggsave("plots/duration_hist_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_dur_hist_nz,
  width = 10, height = 4
)

p_dur_future_nz <- droughts |>
  filter(decarb_scenario == "nz") |>
  plot_duration_by_ba_bars("ba_future")
p_dur_future_nz
ggsave("plots/duration_future_nz_%s.pdf" |> sprintf(plot_drought_type),
  p_dur_future_nz,
  width = 10, height = 4
)

p_dur_hist_bau <- droughts |>
  filter(decarb_scenario == "bau") |>
  plot_duration_by_ba_bars("ba_hist")
p_dur_hist_bau
ggsave("plots/duration_hist_bau_%s.pdf" |> sprintf(plot_drought_type),
  p_dur_hist_bau,
  width = 10, height = 4
)

p_dur_future_bau <- droughts |>
  filter(decarb_scenario == "bau") |>
  plot_duration_by_ba_bars("ba_future")
p_dur_future_bau
ggsave("plots/duration_future_bau_%s.pdf" |> sprintf(plot_drought_type),
  p_dur_future_bau,
  width = 10, height = 4
)



##################################################################################
# spatial connectivity
##################################################################################

day_seq <- seq.POSIXt(
  as.POSIXct("1980-01-01 00:00:00", tz = "UTC"),
  as.POSIXct("2059-01-01 00:00:00", tz = "UTC"),
  by = "days"
)

for (scenario in c("bau", "nz")) {
  for (iyear in c(2020, 2050)) {
    #
    cache_fn <- sprintf("data/connectivity_%s_%s_%s.rda", scenario, plot_drought_type, iyear)
    if (!file.exists(cache_fn)) {
      # compute the events in common between BAs
      multi_ba_events <-
        day_seq |>
        map(function(dayi) {
          x <- droughts |>
            filter(decarb_scenario == scenario, infra_year %in% iyear) |>
            group_by(drought_type, infra_year, weather_year) |>
            filter(datetime_utc == dayi) |>
            pull(ba) |>
            unique() |>
            sort() |>
            as.character()
          if (length(x) > 1) {
            # stop()
            # message(as.character(dayi), " ", paste(x, collapse = " "))
            fromto <- combn(x, 2) |>
              t() |>
              as.data.frame() |>
              rename(ba1 = V1, ba2 = V2)
            order <- fromto |>
              as.matrix() |>
              t() |>
              as.vector() |>
              make_undirected_graph() |>
              largest_component() |>
              gorder()
            data.frame(
              datetime_utc = dayi, ba1 = fromto$ba1, ba2 = fromto$ba2,
              infra_year = iyear,
              # largest number of connected nodes
              largest_component_graph_order = order
            )
          }
        }, .progress = T) |>
        bind_rows() |>
        as_tibble()
      saveRDS(multi_ba_events, cache_fn)
    } else {
      multi_ba_events <- readRDS(cache_fn)
    }
  }
}

for (scenario in c("bau", "nz")) {
  multi_ba_events <- bind_rows(
    readRDS(sprintf("data/connectivity_%s_%s_%s.rda", scenario, plot_drought_type, 2020)),
    readRDS(sprintf("data/connectivity_%s_%s_%s.rda", scenario, plot_drought_type, 2050))
  )
  # add additional info to the connected events
  connected_events_with_edges <- multi_ba_events |>
    # distinct(datetime_utc, largest_component_graph_order) |>
    mutate(
      month = month(datetime_utc),
      season = case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% 3:5 ~ "Spring",
        month %in% 6:8 ~ "Summer",
        month %in% 9:11 ~ "Fall"
      )
    ) |>
    mutate(
      weather_years = ifelse(year(datetime_utc) < 2021, "Historical", "Future"),
      weather_years = factor(weather_years, levels = c("Historical", "Future")),
      season = factor(season, levels = seasons)
    ) #|>
  # filter(!(infra_year==2020 & weather_years == 'Future'))

  # discarding duplicate ba connection info
  connected_events <- connected_events_with_edges |>
    distinct(datetime_utc, largest_component_graph_order, season, weather_years, infra_year)


  percent_similar_days <- connected_events |>
    pivot_wider(
      id_cols = c(datetime_utc, weather_years),
      names_from = infra_year,
      values_from = largest_component_graph_order
    ) |>
    na.omit() |>
    nrow() / nrow(connected_events)

  sprintf(
    "Number of connected events occuring on the same day for %s: %s%%\n",
    scenario, round(100 * percent_similar_days, 1)
  ) |> cat()

  connected_events_binned <- connected_events |>
    group_by(season, weather_years, infra_year, largest_component_graph_order) |>
    summarise(n = length(largest_component_graph_order), .groups = "drop")

  # connected_events_binned |>
  #   ggplot() +
  #   geom_bar(aes(factor(largest_component_graph_order), n, fill = weather_years),
  #     stat = "identity", position = "dodge"
  #   ) +
  #   facet_wrap(~season) +
  #   theme_bw() +
  #   theme(panel.grid.minor = element_blank())

  #######################################################################################
  # difference between future and historical number of connected events
  #######################################################################################
  overall_name <- "Total"
  connected_events_diff <-
    connected_events_with_edges |>
    group_by(infra_year, season, weather_years, largest_component_graph_order) |>
    summarise(n = n()) %>%
    # add in missing rows
    right_join(
      expand.grid(
        largest_component_graph_order = 2:10,
        weather_years = c("Historical", "Future"),
        infra_year = c(2020, 2050),
        season = c(seasons, overall_name)
      ),
      by = join_by(season, weather_years, infra_year, largest_component_graph_order)
    ) |>
    arrange(season, weather_years, infra_year, largest_component_graph_order) |>
    pivot_wider(
      id_cols = c(season, largest_component_graph_order),
      names_from = c(weather_years, infra_year), values_from = n
    ) |>
    mutate(
      climate = Future_2050 - Historical_2050,
      climate = ifelse(is.na(climate), 0, climate),
      infrastructure = Future_2050 - Future_2020,
      infrastructure = ifelse(is.na(infrastructure), 0, infrastructure),
      both = Future_2050 - Historical_2020,
      both = ifelse(is.na(both), 0, both)
    ) %>%
    # add in overall (annual)
    bind_rows(
      . |>
        group_by(largest_component_graph_order) |>
        summarise(
          climate = sum(climate),
          infrastructure = sum(infrastructure),
          both = sum(both)
        ) |>
        mutate(season = overall_name)
    ) |>
    mutate(season = factor(season, levels = c(seasons, overall_name))) |>
    filter(largest_component_graph_order <= 9)

  p_connected_event_diff <- connected_events_diff |>
    # filter(season != "annual") |>
    filter(largest_component_graph_order <= 5) |>
    select(season, largest_component_graph_order, climate, infrastructure) |>
    rename(
      `Climate [Future - Historical]` = climate,
      `Infrastructure [2050 - 2020]` = infrastructure,
      # `Climate and Infrastructure\n[Future2050 - Historical2020]` = both
    ) |>
    pivot_longer(-c(season, largest_component_graph_order)) |>
    ggplot() +
    geom_bar(aes(factor(largest_component_graph_order), value, fill = season),
      position = position_dodge(width = 0.8), stat = "identity", color = "black", width = 0.7
    ) +
    facet_wrap(~name) +
    # geom_text(aes(largest_component_graph_order - 1, diff, label = diff),
    #   position = position_dodge2(width = 0.8),
    #   vjust = 1.5
    #   # data = connected_events_diff |> filter(diff <= 0)
    # ) +
    # geom_text(aes(largest_component_graph_order - 1, diff, label = diff),
    #   position = position_dodge2(width = 0.8),
    #   vjust = -.5,
    #   data = connected_events_diff |> filter(diff > 0)
    # ) +
    theme_bw() +
    scale_fill_manual("", values = c(colorblind_pal()(8)[c(6, 5, 7, 2)], grey(.5))) +
    labs(
      x = "Number of connected BAs per event",
      y = "Difference in the number of connected events"
    ) +
    # scale_y_continuous(breaks = seq(-200, 25, by = 25)) +
    # coord_cartesian(ylim = c(-200, 20)) +
    theme(
      panel.grid.minor = element_blank(),
      # axis.text = element_blank()
      legend.position = "top",
      # strip.background =element_rect(fill="white")
    )
  p_connected_event_diff
  ggsave("plots/connected_event_diff_%s_%s.pdf" |> sprintf(scenario, plot_drought_type),
    p_connected_event_diff,
    width = 10, height = 5
  )

  #######################################################################################
  # spatial connectivity, discarding dates, just counting number of connections
  #######################################################################################
  weather_infra_levels <- c(
    "Historical Weather\n2020 Infrastructure",
    "Future Weather\n2020 Infrastructure",
    "Historical Weather\n2050 Infrastructure",
    "Future Weather\n2050 Infrastructure"
  )

  spatial_connectivity_seasonal <- connected_events_with_edges |>
    group_by(weather_years, infra_year, season, ba1, ba2) |>
    summarise(n = length(datetime_utc), .groups = "drop") |>
    left_join(ba_centroids |> rename(ba1 = ba, lon1 = lon, lat1 = lat), by = join_by(ba1)) |>
    left_join(ba_centroids |> rename(ba2 = ba, lon2 = lon, lat2 = lat), by = join_by(ba2)) |>
    arrange(n) |>
    mutate(
      weather_infra = paste0(weather_years, " Weather\n", infra_year, " Infrastructure"),
      weather_infra = factor(weather_infra, weather_infra_levels)
    )

  exclude_weather_infra <- "" # #"Future Weather\n2020 Infrastructure"

  total_events <- connected_events_with_edges |>
    group_by(infra_year, season, weather_years) |>
    summarise(n = n()) |>
    mutate(
      weather_infra = paste0(weather_years, " Weather\n", infra_year, " Infrastructure"),
      weather_infra = factor(weather_infra, weather_infra_levels)
    ) |>
    mutate(x = -104.4, y = 48.7) |>
    mutate(label = paste(n, "Events")) |>
    filter(weather_infra != exclude_weather_infra)

  spatial_connectivity <- spatial_connectivity_seasonal |>
    group_by(weather_infra, ba1, ba2, lon1, lat1, lon2, lat2) |>
    summarise(n = sum(n), .groups = "drop")

  # filter bas with no connections
  ba_centroids <- ba_centroids |>
    filter(ba %in% (c(
      spatial_connectivity |> pull(ba1),
      spatial_connectivity |> pull(ba2)
    ) |> unique()))


  p_ba_connect <- spatial_connectivity |>
    arrange(ba1) |>
    filter(n > 0) |>
    filter(weather_infra != exclude_weather_infra) |>
    ggplot() +
    with_shadow(
      geom_sf(color = grey(.7), linewidth = .3, fill = "white", data = states_sf),
      alpha = 0,
      sigma = 1,
      x_offset = 5,
      y_offset = 5,
      colour = grey(.9)
    ) +
    # geom_sf(color = grey(.7), linewidth = .3, fill = "white", data = states_sf) +
    geom_segment(aes(lon1, lat1, xend = lon2, yend = lat2, color = n, linewidth = n), alpha = 0.9) +
    scale_color_viridis_c(
      option = "F",
      direction = -1,
      trans = "log",
      breaks = c(1, 2, 5, 10, 20, 50, 100, 150, 200),
      # limits=c(1,170)
      guide = "legend"
    ) +
    scale_linewidth_continuous(
      breaks = c(1, 2, 5, 10, 20, 50, 100, 150, 200),
      range = c(.1, 6)
    ) +
    facet_wrap(~weather_infra) +
    geom_label(aes(lon, lat, label = ba),
      data = ba_centroids, size = 2.5 # ,
    ) +
    theme_minimal() +
    # guides(linewidth = guide_legend(reverse = TRUE)) +
    labs(x = "", y = "", linewidth = "# of Events\nIn Common", color = "# of Events\nIn Common")
  p_ba_connect
  ggsave("plots/spatial_connectivity_%s_%s.pdf" |> sprintf(scenario, plot_drought_type),
    p_ba_connect,
    width = 10, height = 5
  )

  p_ba_connect_season <-
    spatial_connectivity_seasonal |>
    # filter(season == "Fall") |>
    filter(n > 0) |>
    filter(weather_infra != exclude_weather_infra) |>
    # inner_join(total_events,by = join_by(season, weather_infra)) |>
    # mutate(n=n/n_events) |>
    ggplot() +
    with_shadow(
      geom_sf(color = grey(.7), linewidth = .3, fill = gray(.99), data = states_sf),
      alpha = 0,
      sigma = 1,
      x_offset = 5,
      y_offset = 5,
      colour = grey(.9)
    ) +
    # geom_sf(color = grey(.7), linewidth = .3, fill = "white", data = states_sf) +
    coord_sf(xlim = c(-125, -101.5), ylim = c(31, 49.5)) +
    geom_segment(aes(lon1, lat1, xend = lon2, yend = lat2, color = n, linewidth = n), alpha = 1) +
    scale_color_viridis_c(
      option = "F",
      name = "# of Events\nIn Common",
      trans = "log",
      direction = -1,
      breaks = c(1, 2, 5, 10, 25, 50, 100, 200),
      # breaks = c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25),
      guide = "legend"
    ) +
    scale_linewidth_continuous(
      name = "# of Events\nIn Common",
      breaks = c(1, 2, 5, 10, 25, 50, 100, 200),
      # breaks = c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25),
      range = c(.5, 4)
    ) +
    facet_grid(season ~ weather_infra) +
    geom_label(aes(lon, lat, label = ba),
      data = ba_centroids, size = 1.7 # ,
    ) +
    geom_text(aes(x, y, label = label), data = total_events, hjust = 1, vjust = 1, size = 3) +
    # theme_bw() +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    # guides(color=element_text(angle=45))+
    # guides(linewidth = guide_legend(reverse = TRUE)) +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.margin = margin(0, 0, 0, 0, "pt"),
      axis.ticks.length = unit(0, "pt"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank()
    ) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(x = "", y = "")
  p_ba_connect_season
  ggsave("plots/spatial_connectivity_seasonal_%s_%s.pdf" |> sprintf(scenario, plot_drought_type),
    p_ba_connect_season,
    width = 8, height = 8
  )
}

#######################################################################################
# BA maps
#######################################################################################
ba_names <- tribble(
  ~NAME, ~short_name,
  "ARIZONA PUBLIC SERVICE COMPANY", "AZPS",
  "BONNEVILLE POWER ADMINISTRATION", "BPAT",
  "CALIFORNIA INDEPENDENT SYSTEM OPERATOR", "CISO",
  "IDAHO POWER COMPANY", "IPCO",
  "LOS ANGELES DEPARTMENT OF WATER AND POWER", "LDWP",
  "NORTHWESTERN ENERGY (NWMT)", "NWMT",
  "NEVADA POWER COMPANY", "NEVP",
  "PACIFICORP - WEST", "PACW",
  "PACIFICORP - EAST", "PACE",
  "PORTLAND GENERAL ELECTRIC COMPANY", "PGE",
  # "AVANGRID RENEWABLES LLC", #AVA
  "PUBLIC SERVICE COMPANY OF COLORADO", "PSCO",
  "PUBLIC SERVICE COMPANY OF NEW MEXICO", "PNM",
  "PUGET SOUND ENERGY", "PSEI",
  "SALT RIVER PROJECT", "SRP",

  # "WESTERN AREA POWER ADMINISTRATION UGP WEST",
  "AVISTA CORPORATION", "AVA",
  "WESTERN AREA POWER ADMINISTRATION - DESERT SOUTHWEST REGION", "WALC",
  "WESTERN AREA POWER ADMINISTRATION - ROCKY MOUNTAIN REGION", "WACM",
  "TUCSON ELECTRIC POWER COMPANY", "TEPC"
)
ba_sf <- read_sf("/Volumes/data/shapefiles/Control__Areas/Control__Areas2.shp") |>
  st_transform(4326) |>
  left_join(ba_names, by = "NAME") |>
  filter(!is.na(short_name)) |>
  mutate(short_name = factor(short_name, levels = bas_to_include))

p_ba_map <-
  # ggplot(ba_sf)+
  ggplot(ba_sf) +
  # geom_sf(color = grey(.7), linewidth = .3, fill = "white", data = states_sf)+
  geom_sf(aes(fill = NAME), alpha = 0.5) +
  geom_sf_label(aes(label = short_name)) +
  # scale_fill_manual("BA", values = colorblind_pal()(6)[-1]) +
  coord_sf(xlim = c(-124.2, -101.5), ylim = c(31.5, 48.5)) +
  theme_bw() +
  theme(legend.position = "None") +
  labs(x = "", y = "")
p_ba_map
ggsave("plots/map_ba.pdf", p_ba_map, width = 5, height = 4.5, dpi = 600)

p_map_ba_panels <-
  ggplot(ba_sf) +
  geom_sf(color = grey(.7), linewidth = .3, fill = "white", data = states_sf) +
  geom_sf(linewidth = .5, fill = "gray", alpha = 0.5) +
  # geom_sf_label(aes(label = short_name)) +
  # scale_fill_manual("BA", values = colorblind_pal()(6)[-1]) +
  coord_sf(xlim = c(-125, -100), ylim = c(31, 49.1)) +
  theme_bw() +
  facet_wrap(~short_name, nrow = 3) +
  labs(x = "", y = "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0, "pt"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none"
  )
p_map_ba_panels
ggsave("plots/map_ba_panels.pdf", p_map_ba_panels, width = 9, height = 5, dpi = 600)
