source("exec/data_process/constants.R")

validate_internal <- function(df, ...) {
    validate_spp(df)
    validate_health_bounds(df)
    validate_no_duplicate_entries(df)
}

# checks if genera and species are valid, e.g. spelled correctly and known.
# if everything is ok, does nothing.
# if genera/spp are invalid, throws an informative error.
# if you have a valid observation that fails here, add a scientific name to known_spp in the
    # data/constants.toml file.
validate_spp <- function(df, gc, sc, valids = constants$known_spp) {
    
}

# checks to make sure health scores are on the interval [0,10] | NA.
# if everything is ok, does nothing.
# if it finds an invalid value, throws an error.
validate_health_bounds <- function(df, health) {
    bad_inds <- which(
        (df[,health] < 0) | 
        (df[,health] > 10) |
        (!is.na(df[,health])) 
    )

    if(length(bad_inds) > 0) stop("Error: found health scores outside the range of 0-10 or NA.")
}

# checks to make sure each stem only gets 1 entry per survey.
# if everything is ok, does nothing.
# if it finds duplicate entries, throws an informative error.
validate_no_duplicate_entries <- function(df, fqids) {
    num_entries_by_stem <- sapply(split(df, df[,fqids]), nrow)
    offenders <- names()
    if(length(offenders > 0)) {

    }
}

# checks to make sure each tree has no gaps in its stem rank: 
    #   e.g. stems #1 and #3 with no #2 should actually be stems #1 and #2
# if everything is ok, does nothing.
# if it finds gapped stem ranks, throws an informative error. 
validate_max_stem_rank <- function(df) {

}