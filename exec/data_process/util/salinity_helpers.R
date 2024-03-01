# just need clean_path() from exec/data_process/util/helpers.R
source("exec/data_process/util/helpers.R")

format_porewater_2019 <- function(df) {
  df$date <- as.Date(df$date, format = "%m/%d/%y")
  df$zone <- factor(gsub("\\d", "", df$plot), levels = c("H", "M", "L"), ordered = TRUE)
  df$plot <- as.factor(df$plot)
  df$site <- as.factor(rep("BRNV", nrow(df)))
  df$ec_ms_cm <- NA
  df$subject <- factor(rep("porewater", nrow(df)), levels = c("porewater", "soil"))
  cols_to_keep <- c("date", "site", "zone", "plot", "subject", "ppt", "ec_ms_cm")
  df <- df[, cols_to_keep]
  return(df)
}

format_porewater_2023 <- function(df) {
  df$date <- as.Date(df$date, format = "%m/%d/%y")
  df$site <- as.factor(rep("BRNV", nrow(df)))
  df$zone <- factor(df$zone, levels = c("H", "M", "L"), ordered = TRUE)
  df$plot <- as.factor(paste(df$zone, df$plot, sep = ""))
  df$ppt <- df$salinity_ppt
  df$subject <- factor(rep("porewater", nrow(df)), levels = c("porewater", "soil"))
  df$ec_ms_cm <- df$ec_ms.cm
  cols_to_keep <- c("date", "site", "zone", "plot", "subject", "ppt", "ec_ms_cm")
  df <- df[, cols_to_keep]
  return(df)
}

format_soil_salinity_2022 <- function(df) {
  df$date <- as.Date(df$date, format = "%m/%d/%y")
  df$site <- as.factor(rep("BRNV", nrow(df)))
  df$zone <- factor(gsub("\\d", "", df$plot), levels = c("H", "M", "L"), ordered = TRUE)
  df$plot <- as.factor(df$plot)
  df$subject <- factor(rep("soil", nrow(df)), levels = c("porewater", "soil"))
  df$ppt <- NA
  df$ec_ms_cm <- df$ec_ms.cm
  cols_to_keep <- c("date", "site", "zone", "plot", "subject", "ppt", "ec_ms_cm")
  df <- df[, cols_to_keep]
  return(df)
}

format_soil_salinity_2023 <- function(df) {
  df$date <- as.Date(df$date, format = "%m/%d/%y")
  df$zone <- factor(df$zone, levels = c("H", "M", "L"), ordered = TRUE)
  df$plot <- as.factor(df$plot)
  df$site <- as.factor(rep("BRNV", nrow(df)))
  df$subject <- factor(rep("soil", nrow(df)), levels = c("porewater", "soil"))
  df$ppt <- NA
  df$ec_ms_cm <- df$ec_us.cm / 1000
  cols_to_keep <- c("date", "site", "zone", "plot", "subject", "ppt", "ec_ms_cm")
  df <- df[, cols_to_keep]
  return(df)
}

salinity_data_format_map <- list(
  format_porewater_2019,
  format_porewater_2023,
  format_soil_salinity_2022,
  format_soil_salinity_2023
)

# from exec/data_process/util/helpers.R
names(salinity_data_format_map) <- clean_path(list.files("data/raw/salinity"))
