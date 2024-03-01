# adds the utility functions clean_path() and apply_data_formatter_by_name()
source("exec/data_process/util/helpers.R")

# adds utility functions to format each dataset to the global environment
# also adds a map that pairs each dataset with its corresponding formatter function to the env
# this is called salinity_data_format_map
source("exec/data_process/util/salinity_helpers.R")

# read salinity datasets into a list and name them by path
salinity_paths <- list.files("data/raw/salinity", full.names = TRUE)
raw_salinity_data <- lapply(salinity_paths, read.csv)
names(raw_salinity_data) <- clean_path(salinity_paths) # from exec/data_process/util/helpers.R

salinity <- merge(
  # function from exec/data_process/util/helpers.R
  apply_data_formatter_by_name(
    # object from exec/data_process/util/salinity_helpers.R
    format_map = salinity_data_format_map, 
    data_list = raw_salinity_data
  )
  all = TRUE
)
