args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/r/save_tree_census.R <input_csv> <output_rds>")
}

input_csv <- args[1]
output_rds <- args[2]

df <- read.csv(
  input_csv,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

parse_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  uppercase <- toupper(as.character(x))
  out <- rep(NA, length(uppercase))
  out[uppercase %in% c("TRUE", "T", "1", "YES", "Y")] <- TRUE
  out[uppercase %in% c("FALSE", "F", "0", "NO", "N")] <- FALSE
  return(out)
}

df$date <- as.Date(df$date)
df$alive <- parse_logical(df$alive)
df$standing <- parse_logical(df$standing)

numeric_cols <- c("dbh_mm", "health", "old_tag")
df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)

df$tree_number <- as.integer(df$tree_number)
df$trunk_number <- as.integer(df$trunk_number)

# preserve survey order by date, falling back to original order when dates tie
survey_order <- unique(df[order(df$date, df$survey_id), "survey_id"])
df$survey_id <- factor(df$survey_id, levels = survey_order, ordered = TRUE)

df$site <- factor(df$site)
df$zone <- factor(df$zone)
df$plot <- factor(df$plot)
df$tree_code <- as.character(df$tree_code)
df$genus <- as.character(df$genus)
df$spp <- as.character(df$spp)
df$notes <- as.character(df$notes)

saveRDS(df, output_rds)
