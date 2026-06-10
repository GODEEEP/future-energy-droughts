# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Analysis of future compound wind+solar energy droughts across WECC Balancing Authorities (BAs), supporting Bracken et al. "Standardized Benchmark of Historical Compound Wind and Solar Energy Droughts Across the Continental United States." Inactive GODEEEP project; scripts are run interactively rather than as a build pipeline.

## Pipeline

The numbered scripts run in order; each produces inputs for the next. They are slow (hours) and depend on large external data trees, so re-run only the stage that matters.

1. `1-process-data.R` — reads plant-level wind/solar generation from `/Volumes/data/tgw-gen-historical/` and `/Volumes/data/future-wind-solar/`, maps CERF future plants to BAs (nearest existing plant), aggregates to BA-hourly, then collapses to BA-daily. Writes `data/ba-aggregated/ba_{hist,future,expected_future}_{infra_year}_{scenario}_{hourly,daily}.csv`.
2. `2-future-energy-droughts.R` — reads BA-daily files and identifies wind-only, solar-only, and compound wind+solar (`ws`) droughts at the 10th-percentile threshold (SDEI < -1.28). Writes per-BA files into `data/droughts/` then concatenates them per drought type.
3. `3-figures.R` — produces the manuscript figures into `plots/` (a symlink to the Overleaf project at `~/Dropbox/Apps/Overleaf/GODEEEP Future Energy Droughts/figures`).
4. `capacity-table.R` — generates the capacity LaTeX table for the paper.

`B1-droughts.R` is a separate one-off for hydropower (B1) weekly droughts using ORNL EHA hydro plant locations; it does not feed the rest of the pipeline.

## Experimental design

Three "data types" cross two decarbonization scenarios:

- `hist` — future infrastructure (`infra_year` ∈ {2020, 2025, …, 2050}) run through historical climate (1980–2019).
- `future` — future infrastructure run through future climate (2020–2059).
- `expected_future` — each 5-year climate window paired with the matching `infra_year` to approximate the realistic trajectory.
- Scenarios: `bau` (business as usual) and `nz` (net zero).

Important quirk in `lib.R::read_year_and_agg_to_ba`: GCAM retires all EIA plants in 2045, so for `iteration < 2045` the EIA baseline is joined with CERF plants sited up through that year, and from 2045 onward only CERF plants are used. `iteration == 2020` uses EIA plants only.

## Data layout

- `/Volumes/data/tgw-gen-historical/` — historical baseline plant CSVs and `eia_{wind,solar}_configs.csv`.
- `/Volumes/data/future-wind-solar/` — CERF futures (`baseline-historical/`, `baseline-future/`, `cerf-historical-2050/`, `cerf-future-2050/`) plus `cerf-config/{wind,solar}_config_{business_as_usual,net_zero}_ira_ccs_climate_2050.csv`.
- `/Volumes/data/shapefiles/cb_2018_us_state_5m/` — used for figure basemaps.
- `data/` — repo-local working data: `ba-aggregated/` (parquet), `droughts/` (parquet), `cerf/`, `tell/`, BA service-territory CSV, BA centroids, county shapefiles for CERF→BA mapping, and `connectivity_*.rda` cached graph objects for spatial drought connectivity figures. `ba-aggregated/` and `droughts/` use `arrow::read_parquet`/`write_parquet` (zstd compressed); other CSVs in `data/` remain CSV. The one-shot `convert-data-to-parquet.R` script migrated the original CSV outputs.
- `plots/` is a symlink — figures land in the Overleaf project.

If `/Volumes/data/` is not mounted, the processing scripts will fail; figures can still run from cached `data/droughts/` and `data/connectivity_*.rda`.

## Time and standardization conventions

- All BA timestamps are converted to `US/Pacific` after aggregation; this is a deliberate WECC-wide simplification, not a bug.
- `lib.R::sdei` is the SDEI / SREPI: empirical CDF → plotting position → `qnorm`. When all values are zero (e.g., solar at night), it returns `-5` so that compound droughts can continue overnight without breaking the run.
- `run_length` and `run_id` (in `lib.R`) take a `run_diff` argument that is the expected hour gap between consecutive observations — `1` for hourly, `24` for daily. Pass `periodi` not the period name.
- `energy_drought()` finds candidate runs; `energy_drought_filter()` collapses each `run_id` to one row with the mean SDEI severity, summed MWh severity, and metadata. Both must be called as a pair.

## Running

This is plain interactive R; there is no build, lint, or test harness.

- R: `R` (alias `--no-save --no-restore --quiet`); IDE preference is Positron (`p`).
- Required packages: `tidyverse`, `sf`, `fields`, `ggthemes`, `ggfx`, `igraph`, `viridis`, `xtable`, `import`. Install via `install.packages()` as needed.
- To run a single stage: `Rscript 2-future-energy-droughts.R` from the repo root, or step through with `# %%` cell markers in Positron.
- Source `lib.R` at the top of any new script — every analysis script depends on it.

## Code conventions

The repo predates the global R style in `~/CLAUDE.md`. Existing scripts use `<-` for assignment and a mix of `|>` and `%>%`. New code should follow the global style (`=` assignment, native `|>`, `\(x)` lambdas, `# %%` cell markers, `air` formatting at line-width 100), but do not mass-rewrite existing scripts unless asked.
