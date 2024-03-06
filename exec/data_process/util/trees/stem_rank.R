# if a tree_code is in the df, it should be guaranteed to have a "first" stem.
# i.e. a tree_code with only trunk #2 (no #1) is invalid
# if a tree code has multiple stems, they should never be separated by more than one:
# i.e. trunk #1 and trunk #3 without a trunk #2 is invalid

stem_status <- list(
  "has NA in stem"
)

validate_stem_rank <- function(df) {
  xdfl <- split_df(df, filter_trunk = FALSE)

  verdict <- lapply(
    names(xdfl),
    function(x) {
      stems <- get_stems(xdfl[[x]]$trunk_number)
      stem_validator(stems)
    }
  )

  return(verdict)
  violators <- which(unlist(verdict))
  if (length(violators > 0)) {
    error_message <- paste(
      "Stem Rank Error at the following tree codes: ",
      names(xdfl)[violators]
    )
    stop(error_message)
  }
}

get_stems <- function(trunk_num_col) sort(unique(trunk_num_col), na.last = TRUE)

stem_validator <- function(stems) {
  if (
    (any(is.na(stems))) |
      (stems[1] != 1) |
      ((all(diff(stems) != 1)) & (length(stems) > 1))
  ) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
