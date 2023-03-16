#' @title A Function that Computes the Confidence Ellipse Coordinates for Bivariate Normal Data (with Optional Grouping)
#' @author Christian L. Goueguel
#' @description This function computes a confidence ellipse (assuming bivariate normality) at a specified confidence level.
#' @param .data data frame or tibble.
#' @param x the x-axis column name.
#' @param y the y-axis column name.
#' @param conf_level the confidence level for the ellipse (0.95 by default).
#' @param by_group for grouping the bivariate data, if it contains a grouping third column (FALSE by default). Note that this third column must be a factor.
#' @return a data frame of the coordinates points of the ellipse.
#' @export confidence_ellipse
confidence_ellipse <- function(.data, x = NULL, y = NULL, conf_level = 0.95, by_group = FALSE) {

  require(dplyr)
  require(tidyr)
  require(purrr)
  require(forcats)
  require(magrittr)

  # check input validity
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (is.null(x) || is.null(y)) {
    stop("Either 'x' or 'y' argument must be specified.")
  }
  if (!(x %in% colnames(.data) || y %in% colnames(.data))) {
    stop("x, y or both variables do not exist in the input data frame.")
  }
  if (x == y) {
    stop("x and y must be distinct variables.")
  }
  if (!is.numeric(conf_level)) {
    stop("'conf_level' must be numeric.")
  }
  if (conf_level < 0 && conf_level > 1) {
    stop("'conf_level' must be between 0 and 1.")
  }
  if(!is.logical(by_group)) {
    stop("'by_group' must be of boolean type (TRUE or FALSE).")
  }

  # compute ellipse coordinates
  transform_data <- function(.x, conf_level) {
    mean_vec <- colMeans(.x)
    cov_mat <- cov(.x)
    eig <- eigen(cov_mat)

    theta <- (2*pi*seq(0, 360, 1))/360
    B1 <- sqrt(eig$values[1]*qchisq(conf_level, 2)) * cos(theta)
    B2 <- sqrt(eig$values[2]*qchisq(conf_level, 2)) * sin(theta)

    R <- cbind(B1, B2) %*% t(eig$vectors)
    C <- R + matrix(rep(t(mean_vec), 361), ncol = ncol(t(mean_vec)), byrow = TRUE)

    return(C)
  }

  if (by_group == FALSE) {
    X_mat <- .data %>%
      select({{x}}, {{y}}) %>%
      as.matrix()

    Y <- transform_data(X_mat, conf_level)
    Y %<>%
      as_tibble() %>%
      rename(x = V1, y = V2)
  } else {
    if (sum(map_lgl(.data, is.factor)) != 1) {
      stop("The input 'data' must contain exactly one factor column. Currently, there are ", sum(map_lgl(data, is.factor)), " factor columns. Please modify your input to meet the requirement.")
    }
    # Get the names of the factor columns in the data frame
    factor_col <- .data %>%
      select(where(is.factor)) %>%
      names()

    # Group the data by factor columns and nest the data
    nested_data <- df_tbl %>%
      select({{factor_col}}, {{x}}, {{y}}) %>%
      group_by(!!sym(factor_col)) %>%
      nest() %>%
      ungroup()

    Y <- matrix(0, nrow = 361*length(nested_data$data), ncol = 3)

    for (i in seq_along(nested_data$data)) {
      group_data <- nested_data%>%
        pluck(2, i) %>%
        select(where(is.numeric)) %>%
        as.matrix()

      Y_grp <- transform_data(group_data, conf_level)
      Y_grp <- cbind(Y_grp, replicate(361, nested_data$group[i]))
      Y[seq(1+(361*(i-1)), 361*i), ] <- Y_grp
    }
    Y %<>%
      as_tibble() %>%
      rename(x = V1, y = V2, group = V3) %>%
      modify_at("group", as_factor)
  }
  return(Y)
}


