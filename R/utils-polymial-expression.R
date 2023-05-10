make_tidy_data <- function(data, vars, s, k, expressions) {
  features <- purrr::map_chr(expressions, rlang::expr_text, width = 500L)
  total_time <- nrow(data)
  warming_time <- s * (k - 1)
  training_time <- (warming_time + 2):nrow(data)
  df <- tidyr::expand_grid(t = training_time, expr_feature = expressions)

  data_vars <- rlang::as_data_mask(data[, vars])
  df <- df %>%
    dplyr::mutate(value = purrr::map2_dbl(expr_feature, t, function(expr_feature, t) rlang::eval_tidy(expr_feature, data = data_vars, env = rlang::new_environment(list(t = t), globalenv())))) %>%
    tidyr::pivot_wider(id_cols = t, names_from = expr_feature, values_from = value) %>%
    dplyr::bind_cols(data[training_time[1:(length(training_time))], vars])
  return(structure(df, features = features, vars = vars))
}

make_expressions <- function(vars, s, k, p, constant) {
  e_linear <- make_e_linear(vars, s, k)
  e_nonlinear <- make_e_nonlinear(e_linear, s, k, p)
  if (constant) {
    return(c(1, e_linear, e_nonlinear))
  } else {
    return(c(e_linear, e_nonlinear))
  }
}


make_e_linear <- function(vars, s, k) {
  df <- tidyr::expand_grid(vars, 1:k)
  colnames(df) <- c("vars", "k")
  e_linear <- purrr::map2(df$vars, df$k, function(vars, k, s) {
    rlang::expr((!!rlang::sym(vars))[t - !!(s * (k - 1) + 1)])
  }, s = s)
  return(e_linear)
}

make_e_nonlinear <- function(e_linear, s, k, p) {
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
      return(x)
    } else {
      return(rlang::expr((!!x)^(!!y)))
    }
  })
  purrr::reduce(element_list, expression_product)
}

expression_product <- function(a, b) {
  rlang::expr(!!a * !!b)
}
