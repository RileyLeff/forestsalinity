# FUTURE USE: Please leave the underlying data as-is and make any one-off fixes here.
# This approach makes data cleaning reproducible -- crucial for long-term, multi-use datasets.

# If you add a one-off fix, please add a comment describing the rationale.

fix_one_off_problems_manually <- function(df) {
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


  # Future one-off fixes go here

  return(df)
}
