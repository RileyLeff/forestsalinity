library(dplyr)

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

check_alive_vs_health <- function(df) {
  if (nrow(df %>% filter((!alive) & (health > 0))) > 0) {
    stop("Some trees have alive = 0 and health scores > 0. That doesn't make sense.")
  }

  if (nrow(df %>% filter((alive) & (health == 0))) > 0) {
    stop("Some trees are marked alive = 1 and health scores = 0. That doesn't make sense.")
  }
}

cleanup_alive_vs_health <- function(df) {
  # for indices where health is zero but alive is TRUE
  bad_alives <- intersect(which(df$health == 0), which(df$alive))
  # set alive to FALSE
  df$alive[bad_alives] <- FALSE

  # for indices where alive is FALSE but health is not zero
  bad_deads <- intersect(
    which(df$health > 0),
    which(!df$alive)
  )

  # and "dead" is anywhere in the notes for those alive = FALSE and health > 0
  dead_bad_deads <- intersect(
    which(grepl("dead", df$notes)),
    bad_deads
  )

  # set health to zero
  df$health[dead_bad_deads] <- 0

  # for remaining bad_deads, if standing, set alive to true
  for (i in setdiff(bad_deads, dead_bad_deads)) {
    if (df$standing[i] == TRUE) {
      df$alive[i] <- TRUE
    } else {
      stop(
        paste(
          "Error: disagreement between health score, alive status, and standing status at
                    index: ",
          i,
          sep = ""
        )
      )
    }
  }

  return(df)
}

check_if_na_in_alive <- function(df, trunk_num = 1) {
  xdf <- tree_census %>%
    filter(trunk_number == 1) %>%
    arrange(date)
  xdfl <- split(xdf, xdf$tree_code)
  weirds <- lapply(
    xdfl,
    function(x) {
      length(which(is.na(x$alive)))
    }
  )
  if (any(weirds > 0)) {
    stop("Error: no NAs allowed in Alive column. Fix em.")
  }
}
