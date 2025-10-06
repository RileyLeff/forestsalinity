# need clean_path()
source("exec/data_process/helpers.R")

# adds "constants" object to global env
source("exec/data_process/constants.R")

source(
  "exec/data_process/formatters/2023_and_prior/format.R",
)
# need formatter functions for each data file
# lapply(
#   list.files("exec/data_process/formatters", full.names = TRUE), source)

# key each census to its formatter function
tree_census_format_map <- list(
  "trees_2023" = format_trees_2023,
)


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

# sort by date and trunk_number
tree_census <- tree_census[order(tree_census$date, tree_census$trunk_number), ]

# fixes one-offs like swapped tags
source("exec/data_process/trees/one_offs.R")
tree_census <- fix_one_off_problems_manually(tree_census)

# repairs NAs and re-orders trunk # when appropriate.
source("exec/data_process/trees/trunk_number.R")
tree_census <- fix_column_by_map(
  tree_census,
  tree_census$tree_code,
  "trunk_number",
  trunk_na_status_map
)
tree_census <- fix_column_by_map(
  tree_census,
  tree_census$tree_code,
  "trunk_number",
  trunk_number_status_map
)

# when possible, fixes disagreement between alive and health column
source("exec/data_process/trees/alive_and_health_consistent.R")
tree_census <- make_alive_and_health_consistent(tree_census)
check_alive_and_health_are_consistent(tree_census) # will throw error if something wrong w cleanup

# handles missing data in "alive" column, based on the historic/future context of the "tree_code"
source("exec/data_process/trees/alive_na.R")
tree_census <- fix_column_by_map(
  tree_census,
  paste(tree_census$tree_code, tree_census$trunk_number, sep = "_"),
  "alive",
  alive_na_status_map
)

# remove ugly rownames
rownames(tree_census) <- NULL
