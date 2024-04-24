# !!! NOTE: completely arbitrary decision to fill in missing dates with the middle of the month

# .missingdate (and its counterpart .missingmonth) represent mutable global state.
# i.e. they have a side effect on formatter without being included in the function call.

# this is generally bad practice, but we make an exception for this particular case
# because the scope of the variable is limited to just the trees_2023 file.
# if you want to take this variable elsewhere, you gotta refactor to make it a function argument.
.missingdate <- 15

# !!! NOTE: completely arbitrary decision to fill in missing dates with the middle of the year
.missingmonth <- 6

.old_tree_survey_id <- list(
  "June_2019" = list(
    start_date = as.Date(paste("2019", .missingmonth, .missingdate, sep = "-")),
    end_date = as.Date(paste("2019", .missingmonth, .missingdate, sep = "-"))
  ),
  "June_2020" = list(
    start_date = as.Date(paste("2020", .missingmonth, .missingdate, sep = "-")),
    end_date = as.Date(paste("2020", .missingmonth, .missingdate, sep = "-"))
  ),
  "June_2021" = list(
    start_date = as.Date(paste("2021", .missingmonth, .missingdate, sep = "-")),
    end_date = as.Date(paste("2021", .missingmonth, .missingdate, sep = "-"))
  ),
  "June_2022" = list(
    start_date = as.Date("2022-06-15"),
    end_date = as.Date("2022-06-21")
  ),
  "June_2023" = list(
    start_date = as.Date("2023-06-08"),
    end_date = as.Date("2023-06-20")
  )
)

# note that survey map must be in ascending order for the factor conversion to work!
.get_survey_id <- function(datevec, survey_map) {
  ind_map <- lapply(survey_map, \(x) which((datevec >= x$start_date) & (datevec <= x$end_date)))

  # validate that survey_map and datevec are consistent
  # one survey per date
  # no overlapping surveys
  if (all(sort(unlist(ind_map)) == 1:length(datevec))) {
    out_vec <- NA
    # match each survey's set of indices to the original vector structure
    for (i in 1:length(ind_map)) out_vec[ind_map[[i]]] <- names(survey_map)[[i]]
    out_vec <- factor(out_vec, ordered = TRUE)
    return(out_vec)
  } else {
    message <- "survey_map has overlapping dates or a datevec date falls outside of a valid survey_map range"
    stop(message)
  }
}

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

  df$survey_id <- .get_survey_id(df$date, .old_tree_survey_id)

  cols_to_keep <- c(
    "survey_id", "date", "site", "zone", "plot", "tree_number", "tree_code", "trunk_number",
    "genus", "spp", "dbh_mm", "health", "alive", "old_tag", "standing", "notes"
  )

  df <- df[, cols_to_keep]

  return(df)
}
