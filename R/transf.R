

transf <- function(data, var = NULL, quant = 0.99, nbsteps = 2) {

  data %>%
    select(all_of(var)) %>%
    na_if(0) %>%
    cellWise::transfo(
      type = "bestObj",
      robust = TRUE,
      lambdarange = NULL,
      prestandardize = TRUE,
      prescaleBC = FALSE,
      quant = 0.99,
      nbsteps = 2,
      checkPars = list(silent = TRUE)
    ) %T>%
    {
      tibble(
        mineral = vctrs::vec_c(var),
        lambda_hat = pluck(., "lambdahats"),
        transform = pluck(., "ttypes"),
        objective = pluck(., "objective")
      ) %>%
        print()
    } %>%
    pluck("Xt") %>%
    as_tibble()


}
