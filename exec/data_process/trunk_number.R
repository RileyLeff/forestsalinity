trunk_na_status_map <- list(
  "ok" = list(
    condition_check = function(x) all(!is.na(x)),
    handler = NA
  ),
  "all NA" = list(
    condition_check = function(x) all(is.na(x)), # if all are NA
    handler = function(x) {
      return(rep(1, length(x)))
    } # fill NAs with ones
  ),
  "partial NA, rest are one" = list(
    condition_check = function(x) {
      (length(which(is.na(x))) >= 1) & # if at least one NA AND
        (all(x %in% c(1, NA))) & # everything is either NA or one AND
        (!(all(is.na(x)))) # they're not all NA
    },
    handler = function(x) {
      return(rep(1, length(x)))
    } # fill NAs with ones
  )
  ## Future Use: as of 2023, there are zero entries with NA & multiple stems
  ##    and all of the NAs have dbh close to stem #1.
  ## Consider adding logic to assign NA trunk numbers to the closest
  ##    non-NA trunk number in dbh.
  ##  Consider throwing error if the closest
  ##    value is beyond some dbh difference threshold.
)

trunk_number_status_map <- list(
  "ok" = list(
    condition_check = function(x) is_valid_seq_for_trunk_num(as.numeric(x)),
    handler = NA # do nothing
  ),
  "error: contains NA" = list(
    condition_check = function(x) any(is.na(x)),
    handler = function() stop("found NAs in dataset") # throw error
  ),
  "non sequential" = list(
    condition_check = function(x) (!(is_valid_seq_for_trunk_num(as.numeric(x)))),
    handler = function(x) stem_rank_fixer(as.numeric(x))
  )
)

stem_rank_fixer <- function(vec) {
  entries <- sort(unique(vec))
  correct_vals <- 1:length(entries)
  names(correct_vals) <- entries # correct_vals maps actual entries to their correct trunk numbers.
  output <- correct_vals[as.character(vec)]
  names(output) <- NULL
  return(output)
}

is_valid_seq_for_trunk_num <- function(v) {
  if (length(v) == 1) {
    if (v[1] == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if (all(diff(sort(unique(v))) == 1)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
