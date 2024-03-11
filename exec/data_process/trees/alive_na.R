alive_na_status_map <- list(
  "ok" = list(
    condition_check = function(x) all(!is.na(x)),
    handler = NA
  ),
  "all na" = list(
    condition_check = function(x) all(is.na(x)),
    handler = function(x) {
      warning("Found a tree_code x trunk_number combo with all NA in alive column")
      return(x)
    }
  ),
  "starts with NA seq but has other stuff" = list(
    condition_check = function(x) {
      (length(x) > 1) &
        (is.na(x[1])) &
        (!(all(is.na(x[2:length(x)]))))
    },
    handler = function(x) {
      warning(
        "Found a tree_code x trunk_number combo that begins with NA but has other non-NA values"
      )
      return(x)
    }
  ),
  "has na seqs" = list(
    condition_check = function(x) any(is.na(x)) & any(!(is.na(x))),
    handler = function(x) {
      # list of starting points and lengths for sequences of NAs
      na_seqs <- find_contiguous_sequences(which(is.na(x)))

      # adds the context of surrounding census entries for the given tree/trunk combo
      na_seqs <- lapply(na_seqs, identify_na_context, v = x)

      # classifies na_seqs according to status map
      na_seq_cases <- sapply(na_seqs, get_status, lut = na_seq_status_map)

      # apply changes to underlying data based on the map of conditions and handlers
      x <- fix_underlying_data_with_na_seq_context(x, na_seqs, na_seq_cases, na_seq_status_map)

      # return the modified data
      return(x)
    }
  )
)

# current flaw: need to be able to see whole dataset, not just column.
# i.e. check time since last entry (< 1yr), not number of entries since last entry.
na_seq_status_map <- list(
  "between true and true" = list(
    condition_check = function(x) (x$before == "true") & (x$after == "true"),
    handler = function(x) rep(TRUE, length(x))
  ),
  "between true and false" = list(
    condition_check = function(x) (x$before == "true") & (x$after == "false"),
    handler = function(x) rep(FALSE, length(x))
  ),
  "length == 1 between true and end" = list(
    condition_check = function(x) (x$before == "true") & (x$after == "none") & (x$length == 1),
    handler = function(x) rep(NA, length(x))
  ),
  "length > 1 between true and end" = list(
    condition_check = function(x) (x$before == "true") & (x$after == "none") & (x$length > 1),
    handler = function(x) rep(FALSE, length(x))
  ),
  "between false and end" = list(
    condition_check = function(x) (x$before == "false") & (x$after == "none"),
    handler = function(x) rep(FALSE, length(x))
  ),
  "between false and true" = list(
    condition_check = function(x) (x$before == "false") & (x$after == "true"),
    handler = function(x) rep(TRUE, length(x)) # this one questionable ! but likely infrequent
  ),
  "between false and false" = list(
    condition_check = function(x) (x$before == "false") & (x$after == "false"),
    handler = function(x) rep(FALSE, length(x))
  )
)

identify_na_context <- function(na_seq, v) {
  if (na_seq[["start"]] == 1) {
    na_seq$before <- "none"
  } else {
    na_seq$before <- tolower(as.character(v[na_seq[["start"]] - 1]))
  }

  if (na_seq[["start"]] + na_seq[["length"]] - 1 == length(v)) {
    na_seq$after <- "none"
  } else {
    na_seq$after <- tolower(as.character(v[na_seq[["start"]] + na_seq[["length"]]]))
  }
  return(na_seq)
}

fix_underlying_data_with_na_seq_context <- function(x, na_seqs, cases, lut) {
  for (case_ind in 1:length(cases)) {
    start_ind <- na_seqs[[case_ind]]$start
    end_ind <- start_ind + na_seqs[[case_ind]]$length - 1
    x[start_ind:end_ind] <- lut[[cases[case_ind]]]$handler(x[start_ind:end_ind])
  }
  return(x)
}
