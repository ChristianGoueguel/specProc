## code to prepare `specData` dataset

specData <- arrow::read_parquet("~/Documents/Laserag/R&D projects/Soil/Eurofins/SOC/processed data/avg_spec") %>%
  mutate(
    calibration = case_when(
      gdata::startsWith(as.character(sample_id), "SOC") ~ "terraton",
      TRUE ~ "environex"
    )
  ) %>%
  relocate(calibration, .after = spectra_id) %>%
  modify_at("calibration", as_factor) %>%
  filter(calibration %in% "terraton") %>%
  select(-c(qr_code, calibration, sample_id, reception_date:creation_time)) %>%
  modify_at("spectra_id", as.character) %>%
  slice(1:10)

usethis::use_data(specData, overwrite = TRUE)
