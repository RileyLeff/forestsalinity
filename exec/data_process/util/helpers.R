constants <- RcppTOML::parseToml("data/constants.toml")

# example usage:
# clean_path("my/crazy/path/to/a/thing.jpeg")
#
# Output:
# chr
# [1] "thing"
clean_path <- function(path) {
  tools::file_path_sans_ext(
    basename(
      path
    )
  )
}

get_status <- function(d, lut) {
  matches <- sapply(
    names(lut),
    function(status_name) {
      lut[[status_name]]$condition_check(d)
    }
  )

  if (length(which(matches)) != 1) {
    stop(
      paste(
        "ERROR: condition_check TRUE in ",
        length(which(matches)),
        " cases. Data is: ",
        paste(d, collapse = ", "),
        " and the conditions are: ",
        paste(names(matches[which(matches)]), collapse = ", "),
        sep = ""
      )
    )
  }
  return(names(matches)[which(matches)])
}


fix_column_by_map <- function(df, column, lut) {
  xdfl <- split_df(df)

  status <- sapply(
    names(xdfl),
    function(x) {
      get_status(xdfl[[x]][, column], lut)
    }
  )

  needs_fix <- names(status)[which(status != "ok")]

  for (id in needs_fix) {
    # produce a "fixed" column by running the "unfixed" data through the appropriate handler fn
    fixed_column_data <- lut[[status[id]]]$handler(xdfl[[id]][, column])
    xdfl[[id]][, column] <- fixed_column_data
  }

  return(unsplit(xdfl, df$tree_code))
}

# example usage:
#
# fl = list(one = function(x){x+5}, two = function(x){x*5}, three = function(x){x/5})
# dl = list(one = c(1,2,3), two = c(4,5,6), three = c(7,8,9))
#
# apply_data_formatter_by_name(
#   format_map = fl,
#   data_list = dl
# )
#
# Output:
# List
# $one
# [1] 6 7 8
# $two
# [1] 20 25 30
# $three
# [1] 1.4 1.6 1.8
#
# we use names instead of position so things can be out of order, reorganized, etc in future
# if more datasets are added.
apply_data_formatter_by_name <- function(format_map, data_list) {
  Map(
    function(fn, dn) format_map[[fn]](data_list[[dn]]),
    names(format_map), names(data_list)
  )
}

# identify unique subsequences of consecutive integers in vectors by their starting value & length.
# example input: c(1,3,4)
# example output: list where
# [[1]]$start = 1, [[1]]$length = 1
# [[2]]$start = 3, [[2]]$length = 2
find_contiguous_sequences <- function(vec) {
  start_indices <- which(diff(vec) != 1)
  start_indices <- c(1, start_indices + 1)
  end_indices <- c(start_indices[-1] - 1, length(vec))

  result <- list()
  for (i in seq_along(start_indices)) {
    result[[i]] <- list(
      start = vec[start_indices[i]],
      length = end_indices[i] - start_indices[i] + 1
    )
  }
  return(result)
}

split_df <- function(
    df,
    split_by = "tree_code",
    trunk_num = 1,
    sort = TRUE,
    filter_trunk = FALSE) {
  if (filter_trunk) {
    df <- df[df$trunk_number == trunk_num, ]
  }

  if (sort) {
    df <- df[order(df$date), ]
  }

  return(split(df, df[, split_by]))
}
