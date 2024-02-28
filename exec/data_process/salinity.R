raw_data <- lapply(
  X = list.files("data/raw", full.names = TRUE),
  FUN = read.csv
)





salinity <- data.frame(
  date = NA,
  site = NA,
  zone = NA,
  plot = NA,
  subject = NA,
  ppt = NA
)
