specLIBS <-
  readr::read_csv("~/Documents/Projects/Packages/LIBSdata/Ca_Mn_spec.csv") |>
  dplyr::select(x, y0) |>
  tidyr::pivot_wider(names_from = x, values_from = y0)
#####################################################################
DT <-
  data.table::fread(input = "~/Documents/GitHub/cranbSpec.csv") |>
  dplyr::select(-c(qr_code:carrousel_id, replicate:timestamp)) |>
  purrr::modify_at("spectra_id", forcats::as_factor)
cranbSpec <-
  DT[ ,lapply(.SD, mean), by = spectra_id] |>
  tibble::as_tibble()
#####################################################################
soilParam <-
  readr::read_csv("~/Documents/Laserag/Data/OMNIA_validation.csv") |>
  janitor::remove_empty(which = c("rows", "cols")) |>
  janitor::clean_names() |>
  purrr::modify_at("id_supplier", forcats::as_factor) |>
  purrr::modify_at("qr_code", forcats::as_factor) |>
  purrr::modify_at("carrousel_number", forcats::as_factor)
#####################################################################
usethis::use_data(specLIBS, overwrite = TRUE)
usethis::use_data(cranbSpec, overwrite = TRUE)
usethis::use_data(soilParam, overwrite = TRUE)
