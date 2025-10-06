# FUTURE USE: Please leave the underlying data as-is and make any one-off fixes here.
# This approach makes data cleaning reproducible -- crucial for long-term, multi-use datasets.

# If you add a one-off fix, please add a comment describing the rationale.

fix_one_off_problems_2023 <- function(df) {
  df$tree_code <- as.factor(
    paste(
      df$plot,
      df$tree_number,
      substr(df$genus, 1, 3),
      sep = "_"
    )
  )

  # Fix H4 Swap -- H4_508_Pin mixed up with H4_112_Pin.
  #   H4_112_Pin entered survey as dead in 2019, stayed dead in 2020, but had 9 health in 2021.
  #   H4_508_Pin suspiciously similar in 2022 and 2023 -- 352mm dbh health 7 doesn't just pop up
  #     out of nowhere.
  #   Notes say swap them, I agree.
  row_to_swap_into_508 <- which(
    (df$date == as.Date("2021-06-15")) &
      (df$tree_code == "H4_112_Pin")
  )
  df$tree_code[row_to_swap_into_508] <- "H4_508_Pin"
  df$tree_number[row_to_swap_into_508] <- 508

  # Fix Misidentififed Stem On M2_2421_Nys: wrongly labeled as having 3 stems.
  #   "third stem" pops up one year but mysteriously has same dbh as another stem that
  #   was not measured that year. too large to be new growth. notes say two stems.
  row_to_fix_trunk_number <- which(
    (df$date == as.Date("2020-06-15")) &
      (df$tree_code == "M2_2421_Nys")
  )
  df$trunk_number[row_to_fix_trunk_number] <- 2

  # pine M5_2478_Pin has 2023 dbh of 450, not 45. Based on field obs. Makes sense with history.
  df$dbh_mm[which(
    (df$date == as.Date("2023-06-14")) &
      (df$tree_code == "M5_2478_Pin")
  )] <- 450

  # H0 293 was historically misidentified as AcePen. It's a Liq Sty. Something got muddled in the
  # tag change. Fixing here. 
  stop("Fix 293 tag, make AcePen 293 merged with LiqSty 293.")

  return(df)
}
