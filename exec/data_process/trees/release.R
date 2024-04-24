source("exec/data_process/trees/main.R")

output_dir <- "output"
dir.create(output_dir)

write.csv(
  tree_census,
  paste(output_dir, "tree_census.csv", sep = "/")
)

arrow::write_parquet(
  tree_census,
  paste(output_dir, "tree_census.parquet", sep = "/")
)

saveRDS(
  tree_census,
  paste(output_dir, "tree_census.rds", sep = "/")
)
