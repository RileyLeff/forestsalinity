source("exec/data_process/util/helpers.R")

format_porewater_2019 <- function(data) {
  data + 1
}

format_porewater_2023 <- function(data) {
  data + 2
}

format_soil_salinity_2022 <- function(data) {

}

format_soil_salinity_2023 <- function(data) {

}

salinity_data_format_map <- list(
  format_porewater_2019,
  format_porewater_2023,
  format_soil_salinity_2022,
  format_soil_salinity_2023
)

names(salinity_data_format_map) <- tools::file_path_sans_ext(
  basename(
    list.files("data/raw/salinity")
  )
)
