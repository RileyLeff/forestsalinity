format_soil_salinity_2023 <- function(df) {
  badplotids <- which(!(df$plot %in% constants$plot_levels))
  df <- df[-badplotids, ]
  df$date <- as.Date(df$date, format = "%m/%d/%y")
  df$zone <- factor(df$zone, levels = constants$zone_levels, ordered = TRUE)
  df$plot <- factor(df$plot, levels = constants$plot_levels)
  df$site <- as.factor(rep("BRNV", nrow(df)))
  df$subject <- factor(rep("soil", nrow(df)), levels = c("porewater", "soil"))
  df$ec_ms_cm <- df$ec_us.cm / 1000
  df$ppt <- constants$ec_ms_cm_to_ppt_conversion * df$ec_ms_cm
  cols_to_keep <- c("date", "site", "zone", "plot", "subject", "ppt", "ec_ms_cm")
  df <- df[, cols_to_keep]
  return(df)
}
