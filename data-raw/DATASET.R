Ca_Mn_spec <-
  readr::read_csv("~/Documents/Packages/LIBSdata/Ca_Mn_spec.csv") |>
  dplyr::select(x, y0) |>
  tidyr::pivot_wider(names_from = x, values_from = y0)
#####################################################################
DT <-
  data.table::fread(input = "~/Documents/GitHub/cranbSpec.csv") |>
  dplyr::select(-c(qr_code:carrousel_id, replicate:timestamp)) |>
  purrr::modify_at("spectra_id", as_factor)
cranbSpec <-
  DT[ ,lapply(.SD, mean), by = spectra_id] |>
  tibble::as_tibble()
#####################################################################
soilParam <-
  readr::read_csv("~/Documents/Laserag/Data/OMNIA_validation.csv") |>
  janitor::remove_empty(which = c("rows", "cols")) |>
  janitor::clean_names() |>
  purrr::modify_at("id_supplier", as_factor) |>
  purrr::modify_at("qr_code", as_factor) |>
  purrr::modify_at("carrousel_number", as_factor)
#####################################################################
usethis::use_data(Ca_Mn_spec, overwrite = TRUE)
usethis::use_data(cranbSpec, overwrite = TRUE)
usethis::use_data(soilParam, overwrite = TRUE)
