source("exec/data_process/util/trees_helpers.R")

tree_census <- read.csv(
  file = "data/raw/forest_census/trees_2023.csv",
  na.strings = c("NA", "na", "NA ")
)

# sets data types
tree_census <- tree_census_column_cleanup(tree_census)

# fixes one-offs like swapped tags
tree_census <- fix_one_off_problems_manually(df)

# when possible, fixes disagreement between alive and health column
tree_census <- make_alive_and_health_consistent(tree_census)

# categorizes tree codes according to data availability in the alive column
# provides context for cases with NA present
na_map <- na_identifier(df, trunk_num)

# handles missing data in "alive" column.
# optionally interpolates missing dbh and/or health as well in certain cases.
tree_census <- handle_na_cases_by_context(
  df = tree_census,
  trunk_num = 1,
  interpolate = TRUE,
  to_interpolate = c("dbh_mm", "health")
)

# ensures no 0 health scores for alive, no > 0 health for dead. will throw error if violated
check_alive_and_health_are_consistent(tree_census)

# ensures no trees are NA in alive col. will throw error if violated.
check_if_na_in_alive(tree_census, trunk_num = 1)
