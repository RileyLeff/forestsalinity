library(lubridate)

get_mortality_rate <- function(df, from_ts, until_ts, by) {
  from <- year(from_ts)
  until <- year(until_ts)
  yoi <- c(from, until)

  keep_inds <- year(df$date) %in% yoi

  df <- df[keep_inds, ]
  by <- by[keep_inds]

  xdfl <- split(df, by, drop = TRUE)

  lapply(
    names(xdfl),
    function(n) {
      tempdf <- xdfl[[n]]
      ydfl <- split(tempdf, year(tempdf$date))
      common_codes <- intersect(
        unique(ydfl[[1]]$tree_code),
        unique(ydfl[[2]]$tree_code)
      )

      # filters to just tree_codes common to the from year and after year
      lapply(
        names(ydfl),
        function(y) {
          ydfl[[y]] <- ydfl[[y]][which(ydfl[[y]]$tree_code %in% common_codes), ]
        }
      )

      # for the first entry in ydfl, we need to count how many tree_codes have at least one stem marked alive
      # in the second entry in ydfl, we need to count how many tree codes have all dead stems
      # for verification, we can count tree_codes with at least one living stem in the second entry as well to make sure it adds up

      # need to return this in a way that it can be coerced back into a flat df
      # ideal output:

      # output:
      # from, until, by, population, deaths


      # alternatively, could iterate through tree codes, mark a status to each one.
      # probably easier to handle zombies, weird edge cases like that actually.

      # yeah that would make more sense.

      for (tci in 1:length(common_codes)) {

      }
    }
  )
}
