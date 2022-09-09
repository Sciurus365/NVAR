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
#' The feature vector \eqn{\mathbb{O}_{\text {total }}} is then used as input for a ridge regression (by[glmnet::glmnet()]).
#'
#' @param data A matrix or data frame that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @param s The number of time steps used for constructing features.
#' @param k The number of time steps skipped between each two used time steps.
#' @param p The order of polynomial feature vector.
#' @param ... Other parameters passed to [glmnet::glmnet()]
#'
#' @export
#' @references Gauthier, D. J., Bollt, E., Griffith, A., & Barbosa, W. A. S. (2021). Next generation reservoir computing. Nature Communications, 12(1), 5564. https://doi.org/10.1038/s41467-021-25801-2
NVAR <- function(data, vars, s, k, p, ...) {
  data <- as.matrix(data[, vars])
  d <- ncol(data)
  if ((d * k)^p > 100) warning("A large number of features will be created.")

  td <- make_tidy_data(data, vars, s, k, p)

  glmnet::glmnet(alpha = 0, ...)
}

make_tidy_data <- function(data, vars, s, k, p) {
  expressions <- make_expressions(vars, s, k, p)
  total_time <- nrow(data)
  warming_time <- s * (k - 1)
  training_time <- (warming_time+1):nrow(data)
  df <- tidyr::expand_grid(t = training_time, expr_feature = expressions)
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = rlang::eval_tidy(expr_feature, data = data[,vars], rlang::env(t = t))) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(id_cols = t, names_from = expr_feature, values_from = value) %>%
    dplyr::bind_cols(data[training_time,vars])
  return(df)
}

make_expressions <- function(vars, s, k, p) {
  e_linear <- make_e_linear(vars, s, k)
  e_nonlinear <- make_e_nonlinear(e_linear, s, k, p)
  c(e_linear, e_nonlinear)
}


make_e_linear <- function(vars, s, k) {
  df <- tidyr::expand_grid(vars, 1:k)
  colnames(df) <- c("vars", "k")
  e_linear <- purrr::map2(df$vars, df$k, function(vars, k, s) {
    rlang::expr((!!rlang::sym(vars))[t - !!(s * (k - 1))])
  }, s = s)
  return(e_linear)
}

make_e_nonlinear <- function(e_linear, s, k, p) {
  nonlinear_power_vector <- all_vecs(sum = p, length = length(e_linear))
  e_nonlinear <- purrr::map(nonlinear_power_vector, make_monomial, e_linear)
  return(e_nonlinear)
}

#' All vectors with length `length` and sum up to `sum`
all_vecs <- function(sum, length, prev = c()){
  if(length == 1) {
    return(list(c(prev, sum)))
  } else {
    result <- list()
    for (i in sum:0) {
      result <- c(result, all_vecs(sum - i, length - 1, prev = c(prev, i)))
    }
    return(result)
  }
}

make_monomial <- function(power_vector, linear_expressions) {
  le_to_use <- linear_expressions[power_vector != 0]
  pv_to_use <- power_vector[power_vector != 0]
  element_list <- purrr::map2(le_to_use, pv_to_use, function(x, y){
    if(y == 1) {
      return(x)
    } else {
      return(rlang::expr((!!x)^(!!y)))
    }
  })
  purrr::reduce(element_list, expression_product)
}

expression_product <- function(a, b) {
  rlang::expr(!!a*!!b)
}
