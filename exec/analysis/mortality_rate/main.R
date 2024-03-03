source("exec/analysis/zombies/util.R")

get_annual_mortality_rate <- function(df, by, trunk_num = 1, exclude_zombies = TRUE) {
  df <- df[which(df$trunk_number == trunk_num), ]

  if (exclude_zombies) {
    statuses <- get_status_by_tree_code(df, trunk_num)
    df <- df[which(df$tree_code %in% names(statuses[which(statuses != "zombie")])), ]
  }
}
