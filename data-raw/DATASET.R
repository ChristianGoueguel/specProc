## code to prepare `specData` dataset
Ca_Mn_spec <-
  readr::read_csv("~/Documents/Packages/LIBSdata/Ca_Mn_spec.csv") %>%
  dplyr::select(x, y0) %>%
  tidyr::pivot_wider(names_from = x, values_from = y0)

Mg_spec <-
  readr::read_csv("~/Documents/Packages/LIBSdata/Mg_spec.csv") %>%
  dplyr::select(x2, y02) %>%
  tidyr::pivot_wider(names_from = x2, values_from = y02)

usethis::use_data(Ca_Mn_spec, overwrite = TRUE)
usethis::use_data(Mg_spec, overwrite = TRUE)
