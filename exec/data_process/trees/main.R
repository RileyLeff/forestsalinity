source("exec/data_process/util/helpers.R")
source("exec/data_process/trees/helpers.R")

trees_paths <- list.files("data/raw/forest_census", full.names = TRUE)
raw_tree_data <- lapply(trees_paths, read.csv, na.strings = constants$na_reprs)
names(raw_tree_data) <- clean_path(trees_paths) # from exec/data_process/util/helpers.R

# apply formatter functions and bind resulting dataframes together
tree_census <- do.call(
  what = rbind,
  # function from exec/data_process/util/helpers.R
  args = apply_data_formatter_by_name(
    # object from exec/data_process/util/salinity_helpers.R
    format_map = tree_census_format_map,
    data_list = raw_tree_data
  )
)

# fixes one-offs like swapped tags
source("exec/data_process/trees/one_offs.R")
tree_census <- fix_one_off_problems_manually(tree_census)

# repairs NAs and re-orders trunk # when appropriate.
source("exec/data_process/trees/trunk_number.R")
tree_census <- fix_column_by_map(tree_census, "trunk_number", trunk_na_status_map) # for NAs
tree_census <- fix_column_by_map(tree_census, "trunk_number", trunk_number_status_map) # for numbers

# when possible, fixes disagreement between alive and health column
source("exec/data_process/trees/alive_and_health_consistent.R")
tree_census <- make_alive_and_health_consistent(tree_census)

# categorizes tree codes according to data availability in the alive column
# provides context for cases with NA present
na_map <- na_identifier(df, trunk_num)

# handles missing data in "alive" column, based on the historic/future context of the "tree_code"
# optionally interpolates missing dbh and/or health as well in certain cases.
tree_census <- handle_na_cases_by_context(
  df = tree_census,
  na_map = na_map,
  trunk_num = 1,
  interpolate = TRUE,
  to_interpolate = c("dbh_mm", "health"),
  remove_tree_codes_with_all_nas = TRUE,
)

# ensures no 0 health scores for alive, no > 0 health for dead. will throw error if violated
check_alive_and_health_are_consistent(tree_census)

# ensures no trees are NA in alive col. will throw error if violated.
check_if_na_in_alive(tree_census, trunk_num = 1)
