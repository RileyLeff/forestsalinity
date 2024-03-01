clean_path <- function(path) {
  tools::file_path_sans_ext(
    basename(
      path
    )
  )
}
