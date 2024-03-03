source("exec/data_process/main.R")
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
