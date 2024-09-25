# future-energy-droughts

## (optional) Developing aggregated BA data 
Note: This step is optional and only necessary if if you want to reproduce the aggregated ba data from scratch. 

1. Download the historical wind and solar generation data: https://zenodo.org/records/8393319/files/tgw-gen-historical.zip?download=1
2. Download the future wind and solar generation data: https://zenodo.org/records/13717258/files/future-wind-solar.zip?download=1
3. Run the script `1-process-data.R`, this takes several hours to complete. It will produce the directory `data/ba-aggregated` (unless you change the name in the script)

## (optional) Produce energy drought data
Note: this step is optional and only necessary if you want to reproduce the the energy drought data from scratch. 

1. Run the script `2-future-energy-droughts.R`. This will create the directory `data/droughts`.

## Produce figures

1. Run the script `3-figures.R`. This will produce the directory `plots` with all the plots for the manuscript and more that were not included. 