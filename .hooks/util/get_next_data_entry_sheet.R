#source("exec/data_process/main.R") # Keep commented out if reading RDS directly
tree_census <- readRDS("tree_census.rds")
library(stringr)
library(lubridate) # Ensure lubridate is loaded if not already

n <- 2 # Number of years threshold
e <- 25 # Number of extra empty rows
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
        # Filter out stems where all records are older than n years
        if (all((lubridate::year(Sys.Date()) - lubridate::year(y$date)) > n)) {
          return(NULL)
        } else {
          return(y)
        }
      }
    )

    # Remove NULL entries resulting from the filter
    for (i in names(list_of_stems_in_this_plot)) {
      if (is.null(list_of_stems_in_this_plot[[i]])) {
        list_of_stems_in_this_plot[[i]] <- NULL
      }
    }

    # Get the actual number of stems after removing NULLs
    num_stems <- length(list_of_stems_in_this_plot)

    # Initialize output_df with the correct number of rows and appropriate types
    output_df <- data.frame(
      id = character(num_stems),
      dbh = numeric(num_stems),
      health = numeric(num_stems),
      standing = logical(num_stems),
      dhas.1 = character(num_stems),
      dhas.2 = character(num_stems),
      notes = character(num_stems),
      stringsAsFactors = FALSE # Good practice
    )
    # Fill with NAs if explicit NAs are preferred over default empty values
    output_df[,] <- NA


    # Generate IDs only if there are stems
    if (num_stems > 0) {
      good_ids <- lapply(
        list_of_stems_in_this_plot,
        \(stem) {
          # Ensure stem has rows before accessing columns
          if (nrow(stem) > 0) {
            paste(
              substr(stem$genus[1], 1, 3),
              substr(stem$spp[1], 1, 3),
              stem$tree_number[1],
              stem$trunk_number[1],
              sep = "."
            )
          } else {
            NA_character_ # Return NA if stem is empty for some reason
          }
        }
      )

      # Populate the data frame
      for (stem_i in 1:num_stems) {
        # Assign ID
        current_id <- good_ids[[stem_i]][1] # Take first element just in case
        output_df$id[stem_i] <- ifelse(is.na(current_id), "", current_id) # Use empty string if NA

        stem_df <- list_of_stems_in_this_plot[[stem_i]]

        # Process 2023 data
        which2023 <- which(lubridate::year(stem_df$date) == 2023)
        if (length(which2023) > 0) {
          # Take the first record if multiple exist for 2023
          idx2023 <- which2023[1]
          temp_das1 <- paste(
            stem_df[idx2023, "dbh_mm"],
            as.numeric(stem_df[idx2023, "health"]),
            as.numeric(stem_df[idx2023, "alive"]),
            as.numeric(stem_df[idx2023, "standing"]),
            sep = "/"
          )
          output_df$dhas.1[stem_i] <- temp_das1
        } # Implicitly leaves NA if no 2023 data

        # Process 2022 data
        which2022 <- which(lubridate::year(stem_df$date) == 2022)
        if (length(which2022) > 0) {
          # Take the first record if multiple exist for 2022
          idx2022 <- which2022[1]
          temp_das2 <- paste(
            stem_df[idx2022, "dbh_mm"],
            as.numeric(stem_df[idx2022, "health"]),
            as.numeric(stem_df[idx2022, "alive"]),
            as.numeric(stem_df[idx2022, "standing"]),
            sep = "/"
          )
          output_df$dhas.2[stem_i] <- temp_das2
        } # Implicitly leaves NA if no 2022 data

        # Add other relevant info if needed, e.g., old tags
        # output_df$oldtag[stem_i] <- paste(unique(stem_df$old_tag), collapse=", ")
      }
    } # End of if (num_stems > 0)

    # Remove any rows that are completely NA (optional, might not be needed now)
    # output_df <- output_df[rowSums(is.na(output_df)) != ncol(output_df), ]

    # Sort the dataframe by tree number and trunk number
    if (nrow(output_df) > 0 && any(!is.na(output_df$id) & nchar(output_df$id) > 0)) {
        # Use tryCatch for robustness in splitting
        id_parts <- tryCatch({
            stringr::str_split_fixed(output_df$id, "\\.", 4)
        }, error = function(e) {
            warning("Error splitting IDs: ", e$message)
            matrix(NA_character_, nrow = nrow(output_df), ncol = 4) # Return NA matrix on error
        })

        # Ensure id_parts has at least 4 columns before accessing them
        if (ncol(id_parts) >= 4) {
            output_df$tree_num <- suppressWarnings(as.numeric(id_parts[, 3]))
            output_df$trunk_num <- suppressWarnings(as.numeric(id_parts[, 4]))

            # Sort by tree number, then trunk number, handling potential NAs in sorting columns
            output_df <- output_df[order(output_df$tree_num, output_df$trunk_num, na.last = TRUE), ]

            # Remove temporary columns
            output_df$tree_num <- NULL
            output_df$trunk_num <- NULL
        } else {
            warning("ID splitting did not produce enough parts for sorting.")
        }
    }

    # Add extra empty rows at the end
    output_df[(nrow(output_df) + 1):(nrow(output_df) + e), ] <- NA

    return(output_df)
  }
)

# Render R Markdown files for each plot
lapply(
  names(z),
  \(outname){
    print(paste("Processing plot:", outname))
    # Define absolute output paths
    project_root <- getwd() # Assuming the script is run from the project root
    pdf_output_path_abs <- file.path(
      project_root, "datasheets", "datasheetpdfs", paste0(gsub("\\.", "_", outname), ".pdf")
    )
    rmd_output_path_abs <- file.path(
      project_root, "datasheets", "datasheetrmds", paste0(gsub("\\.", "_", outname), ".rmd")
    )
    pdf_output_dir_abs <- dirname(pdf_output_path_abs)
    rmd_output_dir_abs <- dirname(rmd_output_path_abs)

    # Ensure output directories exist
    if (!dir.exists(pdf_output_dir_abs)) {
        print(paste("Creating directory:", pdf_output_dir_abs))
        dir.create(pdf_output_dir_abs, showWarnings = TRUE, recursive = TRUE) # Show warnings now
    }
     if (!dir.exists(rmd_output_dir_abs)) {
        print(paste("Creating directory:", rmd_output_dir_abs))
        dir.create(rmd_output_dir_abs, showWarnings = TRUE, recursive = TRUE) # Show warnings now
    }

    # Replace placeholder in the template
    localsheet <- gsub("PLOTNAMEPLACEHOLDER", outname, basesheet_contents)

    # Write the temporary Rmd file using absolute path
    writeLines(localsheet, rmd_output_path_abs)
    print(paste("Generated Rmd:", rmd_output_path_abs))

    # Explicitly check if PDF directory exists before rendering
    if (!dir.exists(pdf_output_dir_abs)) {
        warning(paste("PDF output directory still does not exist before rendering:", pdf_output_dir_abs))
    } else {
        print(paste("PDF output directory confirmed:", pdf_output_dir_abs))
    }

    # Render the Rmd to PDF using absolute paths
    print(paste("Rendering PDF:", pdf_output_path_abs))
    tryCatch({
        rmarkdown::render(
          rmd_output_path_abs, # Use absolute path for input
          output_file = pdf_output_path_abs, # Use absolute path for output
          envir = new.env(parent = globalenv()) # Render in a clean environment
        )
    }, error = function(e) {
        warning(paste("Failed to render", outname, ":", e$message))
    })
  }
)

print("Script finished.")