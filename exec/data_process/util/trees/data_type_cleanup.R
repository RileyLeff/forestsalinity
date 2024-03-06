tree_census_column_cleanup <- function(df) {
  df$zone <- factor(df$level, levels = constants$zone_levels, ordered = TRUE)

  # !!! NOTE: completely arbitrary decision to fill in missing dates with the middle of the month
  df$day[which(is.na(df$day))] <- 15

  # !!! NOTE: completely arbitrary decision to fill in missing months with the middle of the year
  df$month[which(is.na(df$month))] <- 6

  df$date <- as.Date(
    paste(
      df$year,
      df$month,
      df$day,
      sep = "/"
    )
  )

  df$alive <- as.logical(df$alive)
  df$standing <- as.logical(df$standing)

  df$plot <- factor(df$plot, levels = constants$plot_levels)

  df$spp[which(df$spp == "Unidentifiable")] <- "Unidentifiable spp."
  split_spp <- strsplit(df$spp, split = " ")
  df$genus <- as.factor(sapply(split_spp, `[`, 1))
  df$spp <- as.factor(sapply(split_spp, `[`, 2))

  convert_to_factor <- c("tree_number", "tree_code", "trunk_number", "spp")
  for (column in convert_to_factor) df[, column] <- as.factor(df[, column])

  df$site <- as.factor(rep("BRNV", nrow(df)))

  cols_to_keep <- c(
    "date", "site", "zone", "plot", "tree_number", "tree_code", "trunk_number", "genus", "spp",
    "dbh_mm", "health", "alive", "old_tag", "standing", "notes"
  )

  df <- df[, cols_to_keep]

  return(df)
}
