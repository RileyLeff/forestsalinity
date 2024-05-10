#' Converts survey dataframe into between-survey mortality/survival rate format.
#'
#' @param df A dataframe.
#' @param by A set of column names (must be factors)
#' @return list of named survey pairs, where each survey pair includes a start and an end ID.

get_mortality_data <- function(df, by, survey_id_col, survey_pairs) {
  # throws error if arg "by" is invalid
  validate_input_for_mortality_by(df, by)

  # produce list of dfs representing each survey
  surveydfs <- split(df, survey_id_col)

  # alive
  # alive_previously_dead
  # dead_permanent
  # dead_impermanent
  # dead_recently
}

#' Convenience function to get a list of survey pairs between adjacent surveys.
#'
#' @param df A dataframe.
#' @param surveys A sorted ordered factor vector of unique surveys.
#' @return list of named survey pairs, where each survey pair includes a start and an end ID.
#' @examples
#' # setup
#' df_rows <- 10
#' surveys_in_order <- paste("survey", 1:(df_rows / 2), sep = "_")
#' for (survey in surveys_in_order) survey_ids <- append(survey_ids, rep(survey, 2))
#' df <- data.frame(
#'   thing = rep("cactus", df_rows),
#'   alive = rep(TRUE, df_rows),
#'   survey_id = factor(
#'     survey_ids,
#'     ordered = TRUE,
#'     levels = surveys_in_order
#'   )
#' )
#' # usage
#' get_adjacent_survey_pairs(df, sort(unique(df$survey_id)))
get_adjacent_survey_pairs <- function(df, surveys, name_separator = "_through_") {
  out <- list()
  for (i in 1:(length(surveys) - 1)) {
    this_name <- paste(surveys[i], surveys[i + 1], sep = name_separator)
    out[[this_name]]$start <- surveys[i]
    out[[this_name]]$end <- surveys[i + 1]
  }
  return(out)
}

#' Determine whether the "by" input for the "get_mortality_rate" function is valid.
#'
#' @param df A dataframe.
#' @param by A char vector.
#' @return NULL on success, throws error on failure.
#' @examples
#' x <- data.frame(riley = 1:10, juan = 2:11, jess = 11:20)
#' for (i in 1:length(x)) x[, i] <- as.factor(x[, i])
#' x$justus <- 21:30 # not a factor
#'
#' # returns nothing, valid inputs
#' validate_input_for_mortality_by(x, c("juan", "riley"))
#'
#' # throws error, not a column name in the df input
#' validate_input_for_mortality_by(x, c("juan", "riley", "keryn"))
#'
#' # throws error, not a factor column
#' validate_input_for_mortality_by(x, c("juan", "riley", "justus"))
validate_input_for_mortality_by <- function(df, by) {
  if (!(all(by %in% colnames(df)))) {
    stop("everything in 'by' must be a column name in 'df'")
  }

  for (i in by) if (!("factor" %in% class(df[, i]))) stop("everything in 'by' must be a factor")
}
