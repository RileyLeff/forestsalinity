source("exec/data_process/main.R")

n <- 2
e <- 25
basesheet_path <- "base_next_census.rmd"

con <- file(basesheet_path, "r")

basesheet_contents <- readChar(con, nchars = file.info(basesheet_path)$size)

close(con)

current_wd_for_stupid_rmd <- getwd()


z <- lapply(
  split(
    tree_census,
    interaction(
      tree_census$site,
      tree_census$plot,
      drop = TRUE
    )
  ),
  \(x) { # each x is the chunk of the census in a particular plot

    # each entry in list_of_stems is a stem in a particular plot (duh)
    list_of_stems_in_this_plot <- lapply(
      split(
        x,
        interaction(
          x$tree_code,
          x$trunk_number,
          drop = TRUE
        )
      ),
      \(y) {
        if (all((lubridate::year(Sys.Date()) - lubridate::year(y$date)) > n)) {
          return(NULL)
        } else {
          return(y)
        }
      }
    )

    for (i in names(list_of_stems_in_this_plot)) {
      if (is.null(list_of_stems_in_this_plot[[i]])) {
        list_of_stems_in_this_plot[[i]] <- NULL
      }
    }

    output_df <- data.frame(
      id = NA,
      dbh = NA,
      health = NA,
      alive = NA,
      standing = NA,
      oldtag = NA,
      dhas.1 = NA,
      dhas.2 = NA,
      # ot2 = NA,
      notes = NA
    )



    good_ids <- lapply(
      list_of_stems_in_this_plot,
      \(stem) {
        paste(
          substr(stem$genus[1], 1, 3),
          substr(stem$spp[1], 1, 3),
          stem$tree_number[1],
          stem$trunk_number[1],
          sep = "."
        )
      }
    )

    for (stem_i in 1:length(list_of_stems_in_this_plot)) {
      output_df$id[stem_i] <- good_ids[[stem_i]]

      stem_df <- list_of_stems_in_this_plot[[stem_i]]
      which2023 <- which(lubridate::year(stem_df$date) == 2023)
      if (length(which2023) == 0) {
        output_df$dhas.1[stem_i] <- NA
      } else {
        temp_das1 <- paste(
          stem_df[which2023, "dbh_mm"],
          as.numeric(stem_df[which2023, "health"]),
          as.numeric(stem_df[which2023, "alive"]),
          as.numeric(stem_df[which2023, "standing"]),
          sep = "/"
        )
        output_df$dhas.1[stem_i] <- temp_das1[1]
      }

      which2022 <- which(lubridate::year(stem_df$date) == 2022)
      if (length(which2022) == 0) {
        output_df$dhas.2[stem_i] <- NA
      } else {
        temp_das2 <- paste(
          stem_df[which2022, "dbh_mm"],
          as.numeric(stem_df[which2022, "health"]),
          as.numeric(stem_df[which2022, "alive"]),
          as.numeric(stem_df[which2022, "standing"]),
          sep = "/"
        )
        output_df$dhas.2[stem_i] <- temp_das2[1]
      }
      # output_df$ot2[stem_i] <- list(sort(unique(stem_df$old_tag)))
      output_df[(stem_i + 1), ] <- NA
    }

    output_df[(nrow(output_df) + 1):(nrow(output_df) + e), ] <- NA

    return(output_df)
  }
)

lapply(
  names(z),
  \(outname){
    print(outname)
    stupid_output_path <- paste(
      current_wd_for_stupid_rmd,
      "datasheets",
      paste0(gsub("\\.", "_", outname), ".pdf"),
      sep = "/"
    )
    even_stupider_output_path <- paste(
      current_wd_for_stupid_rmd,
      "datasheets",
      paste0(gsub("\\.", "_", outname), ".rmd")
    )
    localsheet <- gsub("PLOTNAMEPLACEHOLDER", outname, basesheet_contents)
    fp <- even_stupider_output_path
    writeLines(localsheet, fp)
    print(stupid_output_path)
    rmarkdown::render(
      fp,
      output_file = stupid_output_path
    )
  }
)
