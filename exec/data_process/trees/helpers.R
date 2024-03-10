# need clean_path() from exec/data_process/util/helpers.R
source("exec/data_process/util/helpers.R")

# need formatter functions for each data file
lapply(list.files("exec/data_process/trees/formatters", full.names = TRUE), source)

# need salinity conversion and plot/zone levels from data/constants.toml
constants <- RcppTOML::parseToml("data/constants.toml")

tree_census_format_map <- list(
  "trees_2023" = format_trees_2023
)
