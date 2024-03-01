RcppTOML::parseToml("data/constants.toml")

tree_census <- read.csv(
  file = "data/raw/forest_census/trees_2023.csv",
  na.strings = c("NA", "na", "NA ")
)

tree_census$zone <- factor(tree_census$level, levels = constants$zone_levels, ordered = TRUE)

# !!! NOTE: completely arbitrary decision to fill in missing dates with the middle of the month
tree_census$day[which(is.na(tree_census$day))] <- 15

# !!! NOTE: completely arbitrary decision to fill in missing months with the middle of the year
tree_census$month[which(is.na(tree_census$month))] <- 6

tree_census$date <- as.Date(
  paste(
    tree_census$year,
    tree_census$month,
    tree_census$day,
    sep = "/"
  )
)

tree_census$alive <- as.logical(tree_census$alive)
tree_census$standing <- as.logical(tree_census$standing)

tree_census$plot <- factor(tree_census$plot, levels = constants$plot_levels)

convert_to_factor <- c("tree_number", "tree_code", "trunk_number", "spp")
for (column in convert_to_factor) tree_census[, column] <- as.factor(tree_census[, column])

tree_census$site <- as.factor(rep("BRNV", nrow(tree_census)))

cols_to_keep <- c(
  "date", "site", "zone", "plot", "tree_number", "tree_code", "trunk_number", "spp",
  "dbh_mm", "health", "alive", "old_tag", "standing", "notes"
)

tree_census <- tree_census[, cols_to_keep]
