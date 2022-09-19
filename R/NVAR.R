#' Fit a nonlinear vector autoregression model
#'
#' Described by Gauthier et al. (2021), also known as the "next generation reservoir computing" (NG-RC).
#'
#' The feature vector is as follows (from the reference):
#'
#' \deqn{\mathbb{O}_{\text {total }}=\mathbb{O}_{\text {lin }} \oplus \mathbb{O}_{\text {nonlinear }}^{(p)}}
#'
#' \deqn{\mathbb{O}_{\operatorname{lin}, i}=\mathbf{X}_i \oplus \mathbf{X}_{i-s} \oplus \mathbf{X}_{i-2 s} \oplus \ldots \oplus \mathbf{X}_{i-(k-1) s}}
#'
#' \deqn{\mathbb{O}_{\text {nonlinear }}^{(p)}=\mathbb{O}_{\text {lin }}\lceil\otimes\rceil \mathbb{O}_{\text {lin }}\lceil\otimes\rceil \ldots\lceil\otimes\rceil \mathbb{O}_{\text {lin }}}
#'
#' The feature vector \eqn{\mathbb{O}_{\text {total }}} is then used as input for a ridge regression with `alpha`.
#'
#' @param data A `tibble`, data.frame, or matrix that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @param s The number of time steps skipped between each two used time steps.
#' @param k The number of time steps used for constructing features.
#' @param p The order of polynomial feature vector.
#' @param constant Whether there should be a constant value (1) in the feature set? Default is `TRUE`.
#' @param alpha The \eqn{\alpha} value for ridge regression. Default is 0.05.
#'
#' @return An `NVAR` object that contains `data`, `data_td` (a tidy form of `tibble` that contains the training data), `W_out` (the fitted coefficients), and `parameters`.
#'
#' @export
#' @references Gauthier, D. J., Bollt, E., Griffith, A., & Barbosa, W. A. S. (2021). Next generation reservoir computing. Nature Communications, 12(1), 5564. https://doi.org/10.1038/s41467-021-25801-2
NVAR <- function(data, vars, s, k, p, constant = TRUE, alpha = 0.05) {
  data <- tibble::as_tibble(data[, vars])
  d <- ncol(data)
  if ((d * k)^p > 100) warning("A large number of features will be created.")

  expressions <- make_expressions(vars, s, k, p, constant)
  td <- make_tidy_data(data, vars, s, k, expressions)
  Y <- as.matrix(td[, attr(td, "vars")])
  O_total <- as.matrix(td[, attr(td, "features")])
  W_out <- NULL
  W_out <- t(Y) %*% O_total %*% solve(t(O_total) %*% O_total + alpha * diag(ncol(O_total)))

  return(structure(list(data = data, data_td = td, W_out = W_out, expressions = expressions, parameters = list(vars = vars, s = s, k = k, p = p, constant = constant, alpha = alpha)), class = "NVAR"))
}

#' @export
print.NVAR <- function(x, ...) {
  cat("Coefficients:\n")
  print(x$W_out)
}

make_tidy_data <- function(data, vars, s, k, expressions) {
  features <- purrr::map_chr(expressions, rlang::expr_text)
  total_time <- nrow(data)
  warming_time <- s * (k - 1)
  training_time <- (warming_time + 2):nrow(data)
  df <- tidyr::expand_grid(t = training_time, expr_feature = expressions)
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = rlang::eval_tidy(expr_feature, data = data[, vars], rlang::env(t = t))) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(id_cols = t, names_from = expr_feature, values_from = value) %>%
    dplyr::bind_cols(data[training_time[1:(length(training_time))], vars])
  return(structure(df, features = features, vars = vars))
}
