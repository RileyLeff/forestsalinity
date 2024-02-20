se <- function(x) {
  n <- sum(!is.na(x))
  sqrt(var(x, na.rm = T) / n)
}
