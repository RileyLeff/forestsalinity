# this imports global state variables: .missingdate and .missingmonth
# this also imports function .getsurveyid
# this is specific to this dataset --> we are missing some context for the collections before '23.

source("exec/data_process/formatters/2023_and_prior/utils.R")

format_trees_2023 <- function(df) {
  df$zone <- factor(df$level, levels = constants$zone_levels, ordered = TRUE)

  # !!! NOTE: completely arbitrary decision to fill in missing dates with the middle of the month
  df$day[which(is.na(df$day))] <- .missingdate

  # !!! NOTE: completely arbitrary decision to fill in missing months with the middle of the year
  df$month[which(is.na(df$month))] <- .missingmonth

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

  df$survey_id <- .get_survey_id(df$date, .old_tree_survey_id_map)

  cols_to_keep <- c(
    "survey_id", "date", "site", "zone", "plot", "tree_number", "tree_code", "trunk_number",
    "genus", "spp", "dbh_mm", "health", "alive", "old_tag", "standing", "notes"
  )

  df <- df[, cols_to_keep]

  return(df)
}
