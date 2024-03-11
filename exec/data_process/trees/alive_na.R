alive_na_status_map <- list(
  "ok" = list(
    condition_check = function(x) all(!is.na(x)),
    handler = NA
  ),
  "all na" = list(
    condition_check = function(x) all(is.na(x)),
    handler = function(x) {
      print("weird!")
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
      print("really weird!")
      return(x)
    }
  ),
  "has na seqs" = list(
    condition_check = function(x) {
      any(is.na(x)) &
        any(!(is.na(x)))
    },
    handler = function(x) {
      # list of starting points and lengths for sequences of NAs
      na_seqs <- find_contiguous_sequences(which(is.na(x)))

      # adds the context of surrounding census entries for the given tree/trunk combo
      na_seqs <- lapply(na_seqs, identify_na_context, v = x)

      return(fix_underlying_data_with_na_seq_context(x, na_seqs))
    }
  )
)

na_seq_status_map <- list(
  "between true and true" = list(
    condition_check = function(x) (x$before == "true") & (x$after == "true"),
    handler = function(x) rep(TRUE, x$length)
  ),
  "between true and false" = list(
    condition_check = function(x) (x$before == "true") & (x$after == "false"),
    handler = function(x) rep(FALSE, x$length)
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
    condition_check = function(x) (x$before == "false") & (x$after == "true"),
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

fix_underlying_data_with_na_seq_context <- function(x, na_seqs) {

}
