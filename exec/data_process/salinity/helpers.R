# need clean_path() from exec/data_process/util/helpers.R
source("exec/data_process/util/helpers.R")

# need formatter functions for each data file
lapply(list.files("exec/data_process/salinity/formatters", full.names = TRUE), source)

# need salinity conversion and plot/zone levels from data/constants.toml
constants <- RcppTOML::parseToml("data/constants.toml")

salinity_data_format_map <- list(
  "porewater_2019" = format_porewater_2019,
  "porewater_2023" = format_porewater_2023,
  "soil_salinity_2022" = format_soil_salinity_2022,
  "soil_salinity_2023" = format_soil_salinity_2023
)
