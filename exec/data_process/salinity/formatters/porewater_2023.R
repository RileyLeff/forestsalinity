format_porewater_2023 <- function(df) {
  badplotids <- which(!(df$plot %in% constants$plot_levels))
  df <- df[-badplotids, ]
  df$date <- as.Date(df$date, format = "%m/%d/%y")
  df$site <- as.factor(rep("BRNV", nrow(df)))
  df$zone <- factor(df$zone, levels = constants$zone_levels, ordered = TRUE)
  df$plot <- factor(paste(df$zone, df$plot, sep = ""), levels = constants$plot_levels)
  df$ppt <- df$salinity_ppt
  df$subject <- factor(rep("porewater", nrow(df)), levels = c("porewater", "soil"))
  df$ec_ms_cm <- df$ec_ms.cm
  cols_to_keep <- c("date", "site", "zone", "plot", "subject", "ppt", "ec_ms_cm")
  df <- df[, cols_to_keep]
  return(df)
}
