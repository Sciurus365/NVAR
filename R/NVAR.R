#' Estimate Lag-1 Nested Nonlinear Vector Autoregressive Models
#'
#' This function estimate a set of linear and nonlinear autoregressive models, find the best model according to an information criterion, and compare it with the linear AR and VAR models.
#'
#' @param data A `tibble`, data.frame, or matrix that represents a time series of vectors, with each row as a time step.
#' @param vars A character vector of the variable names used in the model.
#' @param p The maximum order of polynomial feature vector.
#' @param ... Parameters passed to [MuMIn::dredge()] for model selection.
#'
#' @return An `NVAR2` object that contains `data`, `data_td` (a tidy form of `tibble` that contains the training data), `W_out` (the fitted coefficients), and `parameters`.
#'
#' @export
NVAR <- function(data, vars, p, ...) {
  data <- tibble::as_tibble(data[, vars, drop = FALSE])
  d <- ncol(data)

  expressions <- make_I_expressions(vars, p)
  formula_all <- construct_formula(vars, expressions)
  td <- make_tidy_data(data, vars, make_e_linear(vars)) # tidy data directly used for model estimation

  result <- lapply(formula_all, function(one_formula){
    MuMIn::dredge(
      lm(one_formula, data = td, na.action = na.fail)
    ) ## not feasible for nvar > 4. Maybe find stepwise solution. It doesn't have to be the best, but relatively okay. I only want to know what's the best AIC. also need to detect AIC for linear VAR and AR.
  })

  return(structure(result, class = "NVAR"))
}

#' @export
print.NVAR <- function(x, ...) {
  cat("An NVAR model with the following coefficients:\n")
  print(x$W_out)
}

#' @export
summary.NVAR <- function(object, ...) {
  cat(sprintf(
    "lambda: %f\nnumber of features: %d\nrmse: %f", object$lambda, as.integer(object$nfeature), object$rmse
  ))
}
