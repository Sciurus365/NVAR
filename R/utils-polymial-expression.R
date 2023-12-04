make_tidy_data <- function(data, vars, expressions) {
  features <- purrr::map_chr(expressions, rlang::expr_text, width = 500L)
  total_time <- nrow(data)
  training_time <- 2:nrow(data)
  df <- tidyr::expand_grid(t = training_time, expr_feature = expressions)

  data_vars <- rlang::as_data_mask(data[, vars])
  env_t <- rlang::new_environment(list(t = NA_integer_), globalenv())
  df <- df %>%
  dplyr::mutate(value = purrr::map2_dbl(expr_feature, t, function(expr_feature, t) {
    env_t$t <- t
    rlang::eval_tidy(expr_feature, data = data_vars, env = env_t)
  })) %>%
  tidyr::pivot_wider(id_cols = t, names_from = expr_feature, values_from = value) %>%
  dplyr::bind_cols(data[training_time[1:(length(training_time))], vars])
  return(structure(df, features = features, vars = vars))
}

#' A wrapper to `make_expressions()`, but add `I()` for each expression.
#' @noRd
make_I_expressions <- function(vars, p) {
  result <- make_expressions(vars, p)
  lapply(result, function(x){
    rlang::expr(I(!!x))
  })
}

make_expressions <- function(vars, p) {
  e_linear <- make_e_linear(vars)
  e_nonlinear <- make_e_nonlinear(e_linear, p)
  return(c(lapply(e_linear, expression_add_backtick), e_nonlinear))
}


make_e_linear <- function(vars) {
  e_linear <- purrr::map(vars, function(vars) {
    rlang::expr((!!rlang::sym(vars))[t - 1])
  })
  return(e_linear)
}

make_e_nonlinear <- function(e_linear, p) {
  nonlinear_power_vector <- all_vecs(sum = p, length = length(e_linear))
  e_nonlinear <- purrr::map(nonlinear_power_vector, make_monomial, e_linear)
  return(e_nonlinear)
}

#' All vectors with length `length` and sum up to `sum`
#' @noRd
all_vecs <- function(sum, length, prev = c()) {
  if (length == 1) {
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
  element_list <- purrr::map2(le_to_use, pv_to_use, function(x, y) {
    if (y == 1) {
      return(expression_add_backtick(x))
    } else {
      return(rlang::expr((!!expression_add_backtick(x))^(!!y)))
    }
  })
  purrr::reduce(element_list, expression_product)
}

expression_product <- function(a, b) {
  rlang::expr(!!a * !!b)
}

expression_add_backtick <- function(x) {
  rlang::parse_expr(paste0("`", rlang::expr_deparse(x), "`"))
}
