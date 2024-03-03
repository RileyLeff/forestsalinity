source("exec/data_process/util/trees_helpers.R")

RcppTOML::parseToml("data/constants.toml")

tree_census <- read.csv(
  file = "data/raw/forest_census/trees_2023.csv",
  na.strings = c("NA", "na", "NA ")
)

tree_census <- tree_census_column_cleanup(tree_census)

tree_census <- cleanup_alive_vs_health(tree_census)

# these will throw an error if data isn't cleaned
check_alive_vs_health(tree_census) # ensures no 0 health scores for alive, no > 0 health for dead
check_if_na_in_alive(tree_census, tree_num = 1) # ensures no trees are NA in alive col
