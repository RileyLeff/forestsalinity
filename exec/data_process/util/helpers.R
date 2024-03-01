# example usage:
# clean_path("my/crazy/path/to/a/thing.jpeg")
#
# Output:
# chr
# [1] "thing"
clean_path <- function(path) {
  tools::file_path_sans_ext(
    basename(
      path
    )
  )
}

# example usage:
#
# fl = list(one = function(x){x+5}, two = function(x){x*5}, three = function(x){x/5})
# dl = list(one = c(1,2,3), two = c(4,5,6), three = c(7,8,9))
#
# apply_data_formatter_by_name(
#   format_map = fl,
#   data_list = dl
# )
#
# Output:
# List
# $one
# [1] 6 7 8
# $two
# [1] 20 25 30
# $three
# [1] 1.4 1.6 1.8
#
# we use names instead of position so things can be out of order, reorganized, etc in future
# if more datasets are added.
apply_data_formatter_by_name <- function(format_map, data_list) {
  Map(
    function(fn, dn) format_map[[fn]](data_list[[dn]]),
    names(format_map), names(data_list)
  )
}
