# future-energy-droughts

This is the code for reproducing the analysis, figures, and tables in the paper:

Bracken, C., Voisin, N., Mongird, K., Burleyson, C. D., & Oikonomou, K. (2025). Intensifying renewable energy droughts in the Western U.S. amid evolving infrastructure and climate. Earth's Future, 13, e2024EF005313. https://doi.org/10.1029/2024EF005313

__NOTE: If you only want to reproduce the figures, Steps 1. and 2. are optional.__

## 1. (optional) Developing aggregated BA data 

_This step is optional and only necessary if if you want to reproduce the aggregated ba data from scratch._

- Download the historical wind and solar generation data: https://zenodo.org/records/8393319/files/tgw-gen-historical.zip?download=1
- Download the future wind and solar generation data: https://zenodo.org/records/13717258/files/future-wind-solar.zip?download=1
- Run the script `1-process-data.R`, this takes several hours to complete. It will produce the directory `data/ba-aggregated` (unless you change the name in the script)

## 2. (optional) Produce energy drought data
Note: this step is optional and only necessary if you want to reproduce the the energy drought data from scratch. 

Run the script `2-future-energy-droughts.R`. This will create the directory `data/droughts`.
    
## 3. Produce figures

First get the `paper` submodule:
```
git submodule update --init --recursive
```

Run the script `3-figures.R`. This will output several plots into the `paper/figures` directory with all the figures for the paper (2-7) and more that were not included in the manuscript (Figure 1 is a manually created flow chart).

## 4. Produce table

Run the script `4-table.R`. This will produce tex code for the table of BA capacities in the paper.