# source("exec/data_process/main.R") # Keep commented out if reading RDS directly
tree_census <- readRDS("tree_census.rds")
library(lubridate) # Ensure lubridate is loaded if not already
library(RcppTOML)

config <- RcppTOML::parseTOML("scripts/config/config.toml")
recent_surveys <- unlist(config$datasheets$recent_prior_surveys)
if (length(recent_surveys) == 0) {
  stop("No recent_prior_surveys defined in scripts/config/config.toml")
}
e <- config$datasheets$extra_blank_rows
if (is.null(e)) {
  e <- 25
}
basesheet_path <- "base_next_census.rmd"

con <- file(basesheet_path, "r")
basesheet_contents <- readChar(con, nchars = file.info(basesheet_path)$size)
close(con)

current_wd_for_stupid_rmd <- getwd()

has_recent_positive_status <- function(stem_df, surveys, column) {
  relevant_rows <- stem_df$survey_id %in% surveys
  if (!any(relevant_rows)) {
    return(FALSE)
  }
  values <- stem_df[relevant_rows, column]
  values <- suppressWarnings(as.logical(values))
  return(any(values %in% TRUE, na.rm = TRUE))
}

should_include_stem <- function(stem_df, recent_surveys) {
  alive_recent <- has_recent_positive_status(stem_df, recent_surveys, "alive")
  standing_recent <- has_recent_positive_status(stem_df, recent_surveys, "standing")
  return(alive_recent || standing_recent)
}

format_dhas_entry <- function(row, index) {
  values <- c(
    row[index, "dbh_mm"],
    row[index, "health"],
    row[index, "alive"],
    row[index, "standing"]
  )

  formatted <- vapply(
    values,
    function(val) {
      if (is.na(val)) {
        return("")
      }
      if (is.logical(val)) {
        return(as.character(as.integer(val)))
      }
      if (is.numeric(val)) {
        return(as.character(val))
      }
      return(as.character(val))
    },
    character(1)
  )

  if (all(formatted == "")) {
    return("")
  }
  paste(formatted, collapse = "/")
}

split_ids <- function(vec, n = 4) {
  splits <- strsplit(vec, "\\.")
  padded <- lapply(
    splits,
    function(parts) {
      if (length(parts) >= n) {
        return(parts[seq_len(n)])
      }
      c(parts, rep("", n - length(parts)))
    }
  )
  do.call(rbind, padded)
}

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
        if (!any(y$survey_id %in% recent_surveys)) {
          return(NULL)
        }
        if (!should_include_stem(y, recent_surveys)) {
          return(NULL)
        }
        return(y)
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
      stnd = character(num_stems),
      p = character(num_stems),
      dhas.1 = character(num_stems),
      dhas.2 = character(num_stems),
      notes = character(num_stems),
      stringsAsFactors = FALSE # Good practice
    )
    # Fill with NAs if explicit NAs are preferred over default empty values
    output_df[, ] <- NA


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

        primary_survey <- recent_surveys[1]
        secondary_survey <- if (length(recent_surveys) >= 2) recent_surveys[2] else NA

        # Process most recent survey
        which_primary <- which(stem_df$survey_id == primary_survey)
        if (length(which_primary) > 0) {
          idx_primary <- which_primary[1]
          output_df$dhas.1[stem_i] <- format_dhas_entry(stem_df, idx_primary)
        } # Implicitly leaves NA if no data for the primary survey

        # Process second-most recent survey when defined
        which_secondary <- which(stem_df$survey_id == secondary_survey)
        if (!is.na(secondary_survey) && length(which_secondary) > 0) {
          idx_secondary <- which_secondary[1]
          output_df$dhas.2[stem_i] <- format_dhas_entry(stem_df, idx_secondary)
        } # Implicitly leaves NA if no data for the secondary survey

        # Add other relevant info if needed, e.g., old tags
        # output_df$oldtag[stem_i] <- paste(unique(stem_df$old_tag), collapse=", ")
      }
    } # End of if (num_stems > 0)

    # Remove any rows that are completely NA (optional, might not be needed now)
    # output_df <- output_df[rowSums(is.na(output_df)) != ncol(output_df), ]

    # Sort the dataframe by tree number and trunk number
    if (nrow(output_df) > 0 && any(!is.na(output_df$id) & nchar(output_df$id) > 0)) {
      # Use tryCatch for robustness in splitting
      id_parts <- tryCatch(
        {
          split_ids(output_df$id, 4)
        },
        error = function(e) {
          warning("Error splitting IDs: ", e$message)
          matrix(NA_character_, nrow = nrow(output_df), ncol = 4) # Return NA matrix on error
        }
      )

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

included_counts <- sapply(
  z,
  \(df) sum(!is.na(df$id) & nchar(df$id) > 0)
)
print("Included stems per plot (pre-blanks):")
print(included_counts)

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
    tryCatch(
      {
        rmarkdown::render(
          rmd_output_path_abs, # Use absolute path for input
          output_file = pdf_output_path_abs, # Use absolute path for output
          envir = new.env(parent = globalenv()) # Render in a clean environment
        )
      },
      error = function(e) {
        warning(paste("Failed to render", outname, ":", e$message))
      }
    )
  }
)

print("Script finished.")
has_recent_positive_status <- function(stem_df, surveys, column) {
  inds <- stem_df$survey_id %in% surveys
  if (!any(inds)) {
    return(FALSE)
  }
  vals <- stem_df[inds, column]
  vals <- suppressWarnings(as.logical(vals))
  return(any(vals %in% TRUE, na.rm = TRUE))
}

should_include_stem <- function(stem_df, recent_surveys) {
  alive_recent <- has_recent_positive_status(stem_df, recent_surveys, "alive")
  standing_recent <- has_recent_positive_status(stem_df, recent_surveys, "standing")
  return(alive_recent || standing_recent)
}
