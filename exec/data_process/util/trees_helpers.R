check_alive_vs_health <- function(df) {
  if (nrow(df %>% filter((!alive) & (health > 0))) > 0) {
    stop("Some trees have alive = 0 and health scores > 0. That doesn't make sense.")
  }

  if (nrow(df %>% filter((alive) & (health == 0))) > 0) {
    stop("Some trees are marked alive = 1 and health scores = 0. That doesn't make sense.")
  }
}

cleanup_alive_vs_health <- function(df) {
  # for indices where health is zero but alive is TRUE
  bad_alives <- intersect(which(df$health == 0), which(df$alive))
  # set alive to FALSE
  df$alive[bad_alives] <- FALSE

  # for indices where alive is FALSE but health is not zero
  bad_deads <- intersect(
    which(df$health > 0),
    which(!df$alive)
  )

  # and "dead" is anywhere in the notes for those alive = FALSE and health > 0
  dead_bad_deads <- intersect(
    which(grepl("dead", df$notes)),
    bad_deads
  )

  # set health to zero
  df$health[dead_bad_deads] <- 0

  # for remaining bad_deads, if standing, set alive to true
  for (i in setdiff(bad_deads, dead_bad_deads)) {
    if (df$standing[i] == TRUE) {
      df$alive[i] <- TRUE
    } else {
      stop(
        paste(
          "Error: disagreement between health score, alive status, and standing status at
                    index: ",
          i,
          sep = ""
        )
      )
    }
  }

  return(df)
}
