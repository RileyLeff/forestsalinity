library(dplyr, quietly = TRUE)

constants <- RcppTOML::parseToml("data/constants.toml")

tree_census_column_cleanup <- function(df) {
  df$zone <- factor(df$level, levels = constants$zone_levels, ordered = TRUE)

  # !!! NOTE: completely arbitrary decision to fill in missing dates with the middle of the month
  df$day[which(is.na(df$day))] <- 15

  # !!! NOTE: completely arbitrary decision to fill in missing months with the middle of the year
  df$month[which(is.na(df$month))] <- 6

  df$date <- as.Date(
    paste(
      df$year,
      df$month,
      df$day,
      sep = "/"
    )
  )

  df$alive <- as.logical(df$alive)
  df$standing <- as.logical(df$standing)

  df$plot <- factor(df$plot, levels = constants$plot_levels)

  df$spp[which(df$spp == "Unidentifiable")] <- "Unidentifiable spp."
  split_spp <- strsplit(df$spp, split = " ")
  df$genus <- as.factor(sapply(split_spp, `[`, 1))
  df$spp <- as.factor(sapply(split_spp, `[`, 2))

  convert_to_factor <- c("tree_number", "tree_code", "trunk_number", "spp")
  for (column in convert_to_factor) df[, column] <- as.factor(df[, column])

  df$site <- as.factor(rep("BRNV", nrow(df)))

  cols_to_keep <- c(
    "date", "site", "zone", "plot", "tree_number", "tree_code", "trunk_number", "genus", "spp",
    "dbh_mm", "health", "alive", "old_tag", "standing", "notes"
  )

  df <- df[, cols_to_keep]

  return(df)
}

fix_one_off_problems_manually <- function(df) {
  # FUTURE USE: Please leave the underlying data as-is and make any one-off fixes here.
  # This approach makes data cleaning reproducible -- crucial for long-term, multi-use datasets.

  # Fix H4 Swap -- H4_508_Pin mixed up with H4_112_Pin.
  #   H4_112_Pin entered survey as dead in 2019, stayed dead in 2020, but had 9 health in 2021.
  #   H4_508_Pin suspiciously similar in 2022 and 2023 -- 352mm dbh health 7 doesn't just pop up
  #     out of nowhere.
  #   Notes say swap them, I agree.
  row_to_swap_into_508 <- which((df$date == as.Date("2021-06-15")) & (df$tree_code == "H4_112_Pin"))
  df$tree_code[row_to_swap_into_508] <- "H4_508_Pin"
  df$tree_number[row_to_swap_into_508] <- 508
  return(df)
}

check_alive_and_health_are_consistent <- function(df) {
  if (nrow(df %>% filter((!alive) & (health > 0))) > 0) {
    stop("Some trees have alive = 0 and health scores > 0. That doesn't make sense.")
  }

  if (nrow(df %>% filter((alive) & (health == 0))) > 0) {
    stop("Some trees are marked alive = 1 and health scores = 0. That doesn't make sense.")
  }
}

make_alive_and_health_consistent <- function(df) {
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

check_if_na_in_alive <- function(df, trunk_num = 1) {
  xdf <- tree_census %>%
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

find_contiguous_sequences <- function(vec) {
  start_indices <- which(diff(vec) != 1)
  start_indices <- c(1, start_indices + 1)
  end_indices <- c(start_indices[-1] - 1, length(vec))
  
  result <- list()
  for (i in seq_along(start_indices)) {
    result[[i]] <- list(start = vec[start_indices[i]], 
                        length = end_indices[i] - start_indices[i] + 1)
  }

  return(result)
}

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

# finds tree codes with NA in their history, categorizes them based on surrounding years' context.
# will identify most issues with missing data in the alive, dbh, and health columns.
# includes weird edge cases.
na_identifier <- function(df, trunk_num) {
  xdf <- tree_census %>%
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

handle_na_cases_by_context <- function(
  df, 
  na_map,
  trunk_num = 1,
  interpolate = TRUE, 
  to_interpolate = c(),
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

interpolate_missing_data <- c(list_of_dfs, picks){
  check_if_interpolation_ok(df, to_interpolate)
}

# oh my god i could use the same thing i did for the salinity read in where it's a map of functions
