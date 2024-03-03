library(dplyr)
library(ggplot2)

x <- tree_census %>%
  filter(trunk_number == 1) %>%
  filter(!is.na(alive)) %>%
  arrange(date)
x <- split(x, x$tree_code)


# any subdf where alive is false at any point in the df but true in any subsequent ones

y <- lapply(
  X = x,
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
