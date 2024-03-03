# for now, always read in data when running zombies
source("exec/data_process/main.R")

# adds get_tree_codes_by_status() to environment
source("exec/analysis/zombies/util.R")

zombie_tree_codes <- get_tree_codes_by_status(
  df = tree_census,
  trunk_num = 1,
  status = "zombie"
)

dead_tree_codes <- get_tree_codes_by_status(
  df = tree_census,
  trunk_num = 1,
  status = "dead non-zombie"
)
