

shapiroWilks_test <- function(data, var = NULL) {

  data %>%
    select(all_of(var)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    group_by(variable) %>%
    rstatix::shapiro_test(value) %>%
    select(-variable) %>%
    ungroup()
}
