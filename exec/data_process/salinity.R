# adds the utility functions to format each dataset to the global environment
# also adds a map that pairs each dataset with its corresponding formatter function to the env
source("exec/data_process/util/salinity_helpers")

# read salinity datasets into a list and name them
salinity_paths <- list.files("data/raw/salinity", full.names = TRUE)
raw_salinity_data <- lapply(salinity_paths, read.csv)
names(raw_salinity_data) <- basename(salinity_paths)

salinity <- data.frame(
  date = NA,
  site = NA,
  zone = NA,
  plot = NA,
  subject = NA,
  ppt = NA
)
