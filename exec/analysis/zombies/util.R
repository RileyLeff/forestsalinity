library(dplyr)

# returns a vector of tree IDs that demonstrate zombification at any point in the survey
# status is one of "zombie", "dead non-zombie", or "alive"
get_tree_codes_by_status <- function(df, trunk_num = 1, status = "zombie") {
  statuses <- get_status_by_tree_code(df, trunk_num)
  names(statuses[which(statuses == status)])
}

# returns a list that maps tree_code as a status.
# status is a character vector, one of "alive", "dead non-zombie", or "zombie" as a value.
get_status_by_tree_code <- function(df, trunk_num = 1) {
  filtered_df <- as.data.frame(
    df %>%
      filter(trunk_number == trunk_num) %>%
      filter(!is.na(alive)) %>%
      arrange(date)
  )
  status_by_code_map <- .internal_status_finder(
    split(
      x = filtered_df,
      f = filtered_df$tree_code
    )
  )
}

# don't call this function directly, it will be get confused by trunk_number
# interact with this through the wrapper get_zombie_tree_codes()
.internal_status_finder <- function(df) {
  lapply(
    X = df,
    FUN = function(x) {
      # find instances logged as alive = FALSE
      dead_inds <- which(!x$alive)

      # if there are none, it's still alive
      if (length(dead_inds) == 0) {
        return("alive")

        # else there's at least one instance of dead
      } else {
        # if all rows after first appearance of alive = FALSE are also marked alive = FALSE
        if (identical(dead_inds, dead_inds[1]:nrow(x))) {
          return("dead non-zombie")
        } else {
          return("zombie")
        }
      }
    }
  )
}
