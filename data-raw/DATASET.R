## code to prepare `specData` dataset
specData <- arrow::read_parquet("~/Documents/Packages/LIBSdata/specData")
usethis::use_data(specData, overwrite = TRUE)
