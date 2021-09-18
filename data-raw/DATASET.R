## code to prepare `specData` dataset
Ca_Mn_spec <- read_csv("~/Documents/Packages/LIBSdata/Ca_Mn_spec.csv")
Mg_spec <- read_csv("~/Documents/Packages/LIBSdata/Mg_spec.csv")
usethis::use_data(Ca_Mn_spec, overwrite = TRUE)
usethis::use_data(Mg_spec, overwrite = TRUE)
