status_map <- list(
    "no na seqs" = list(
        condition_check = function(x) all(!is.na(x)),
        handler = function(x) {} # do nothing
    ),
    "all na" = list(
        condition_check = function(x) all(is.na(x)),
        handler = NA
    ),
    "starts with na seq but has other stuff" = list(
        condition_check = function(x) {
            (length(x) > 1) &
            (is.na(x[1])) &
            (!(all(is.na(x[2:length(x)]))))
        },
        handler = NA
    ),
    "has na seqs" = list(
        condition_check = function(x) {
            any(is.na(x)) &
            any(!(is.na(x)))
        }
    )
)

na_seq_status_map <- list(
    "between true and true" = list(
        condition_check = function(x) return(NA)
        handler = NA
    ),
    "between true and false" = list(
        condition_check = NA,
        handler = NA
    ),
    "length == 1 between true and end" = list(
        condition_check = NA,
        handler = NA
    ),
    "length > 1 between true and end" = list(
        condition_check = NA,
        handler = NA
    ),
    "between false and end" = list(
        condition_check = NA,
        handler = NA
    ),
    "between false and true" = list(
        condition_check = NA,
        handler = NA
    ),
    "between false and false" = list(
        condition_check = NA,
        handler = NA
    ),
)




identify_na_context <- function(na_seq, df) {
    if(na_seq[["start"]] == 1) {
        na_seq$before = "none"
    } else {
        na_seq$before = df$alive[na_seq[["start"]] - 1]
    }

    if(na_seq[["start"]] + na_seq[["length"]] - 1 == nrow(df)) {
        na_seq$after = "none"
    } else {
        na_seq$after = df$alive[na_seq[["start"]] + na_seq[["length"]]]
    }
    return(na_seq)
}

status_validity_checker <- function(df, status_map, trunk_num) {
  xdfl <- create_filtered_split_df(df,"tree_code", trunk_num, TRUE, TRUE)
  lapply(
    status_map, # for each entry in status_map
    function(this_status){
        lapply(     # for each tree_code in the split list of dataframes
            xdfl,
            function(data_by_tree_code) {
                this_status$condition_check(data_by_tree_code$alive)
            }
        )
    }
  )
}


# finds tree codes with NA in their history, categorizes them based on surrounding years' context.
# will identify most issues with missing data in the alive, dbh, and health columns.
# includes weird edge cases.
na_identifier <- function(df, trunk_num) {
  xdf <- df %>%
    filter(trunk_number == trunk_num) %>%
    arrange(date)
  xdfl <- split(xdf, xdf$tree_code)
  output <- lapply(
    xdfl,
    function(x) {

        ## Special Cases: 

        # if there are no NAs
        if(!any(is.na(x$alive))) return("ok")

        # if all entries are NAs
        else if (all(is.na(x$alive))) return("all are na")

        # if earliest entry is NA
        else if (is.na(x$alive[1])) return("starts with na but has other stuff")

        ## Now, we know 
        ## 1.) there is at least one na in any instances that haven't returned yet
        ## 2.) that na is not the first row of the dataframe
        ## 3.) there's at least one real TRUE or FALSE adjacent to the NA(s)

        ## General Cases:
        ## We return a list:
            ## Each entry in the list is one of these:
                ## 1.) Starting index of the NA sequence
                ## 2.) length of the NA sequence
                ## 3.) whether it is preceded by TRUE or FALSE
                ## 4.) whether it is followed by TRUE, FALSE, or None (i.e. it is last)

        na_inds <- which(is.na(x$alive))
        na_seqs <- find_contiguous_sequences(na_inds) # this grabs starting index and length
        return(lapply(na_seqs, identify_na_context, df = x)) # this adds before and after context
    }
  )
  return(output)
}

check_if_interpolation_ok <- function(df, cols) {
  if(!(all(cols %in% colnames(df)))) {
    stop("Trying to interpolate a column that doesn't exist")
  }
  return(TRUE)
}

# maps possible NA statuses to a handler and a condition
status_map <- 


handle_na_cases_by_context <- function(
  df, 
  na_map,
  trunk_num = 1,
  interpolate = TRUE, 
  to_interpolate = c("dbh_mm", "health"),
  remove_tree_codes_with_all_nas = TRUE
){

  xdf <- df %>%
    filter(trunk_number == trunk_num) %>%
    arrange(date)
  xdfl <- split(xdf, xdf$tree_code)

  for(tree_code in names(na_map)) {

    outer <- na_map[[tree_code]]

    # skip to next iteration if not a NA list
    if(outer == "ok") {
      next
    }

    if(outer == "starts with na but has other stuff") {
      stop("Can't have instances of starts with na but has other stuff")
    }

    if(outer == "all are na") {
       next
     }

    # nested loop is kinda blah but these sublists should typically be of length 1
    for(x in outer){

      if(x$before == "none") stop("Can't have NA as the start of a tree subdf.")

      if(x$before){

        if(x$after == "none"){

          # case: true before, none after AND length of contiguous NA sequence is == 1
          # set to alive, leave dbh and health as NA
          # add note that says it's currently missing
          if(x$length == 1) {
            xdfl[[tree_code]]$alive[x$start:(x$start + x$length - 1)] <- TRUE
            current_note <- xdfl[[tree_code]]$notes[nrow(xdfl[[tree_code]])]
            new_note <- paste(
              current_note,
              " // AUTOCLEANER: missing as of last year //"
            )
            xdfl[[tree_code]]$notes[nrow(xdfl[[tree_code]])] <- new_note
          
          # case: true before, none after AND length of contiguous NA sequence is > 1
          # set to dead
          } else {
            xdfl[[tree_code]]$alive[x$start:(x$start + x$length - 1)] <- FALSE
          }

        # case: true before, true after
        # set to alive, interpolate dbh and health linearly
        } else if(x$after){
            xdfl[[tree_code]]$alive[x$start:last] <- TRUE

            # if we want to interpolate any columns
            if(interpolate){

            # for each column we're interested in interpolating
              for(j in interpolate){

                j_before <-  xdfl[[tree_code]][x$start - 1, j]
                j_after <-  xdfl[[tree_code]][x$start + x$length, j]

                # if measurement col (dbh or health) is not NA before or after
                if(!((is.na(j_before)) & (is.na(j_after)))) {

                  # interpolate between the column's values before and after the NAs
                  # note that interpolated vals includes the "before" and "after" values as well
                  interpolated_vals <- seq(j_before, j_after, length.out = x$length + 2)

                  # replace them in the dataframe
                  xdfl[[tree_code]][(x$start - 1):(x$start + x$length), j] <- interpolated_vals

                  # add note that says interpolated
                  current_notes <- xdfl[[tree_code]]$notes[x$start:(x$start + x$length - 1)]
                  new_notes <- paste(
                    current_notes,
                    "// AUTOCLEANER: interpolated column:",
                    j,
                    sep = " "
                  )

                  xdfl[[tree_code]]$notes[x$start:(x$start + x$length - 1)] <- new_notes

                }
              }

            }

        # case: true before, false after
        # set to dead
        } else {
            xdfl[[tree_code]]$alive[x$start:(x$start + x$length - 1)] <- FALSE
        }
        
      # case: false before
      # set to dead

      } else if(!(x$before)) {
        last <- x$start + x$length - 1
        xdfl[[tree_code]]$alive[x$start:last] <- FALSE
      }
    } 
  }
  outdf <- unsplit(xdfl, xdf$tree_code)
  return(outdf)
}

check_if_na_in_alive <- function(df, trunk_num = 1) {
  xdf <- df %>%
    filter(trunk_number == trunk_num) %>%
    arrange(date)
  xdfl <- split(xdf, xdf$tree_code)
  weirds <- lapply(
    xdfl,
    function(x) {
      length(which(is.na(x$alive)))
    }
  )
  if (any(weirds > 0)) {
    stop("Error: no NAs allowed in Alive column.")
  }
}

interpolate_missing_data <- c(list_of_dfs, picks){
  check_if_interpolation_ok(df, to_interpolate)
}

# oh my god i could use the same thing i did for the salinity read in where it's a map of functions
