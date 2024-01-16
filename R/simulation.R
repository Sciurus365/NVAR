#' Time series simulation with an NVAR model
#'
#' @param model An `NVAR` model, fitted by [NVAR()].
#' @param init A `tibble`, data.frame, or matrix that specify the initial values for a simulation. Should contain the variables used to fit the model and be at least \eqn{s * (k - 1)} long. `NULL` by default, in which case the data used for fitting the model will be used for simulation.
#' @param length How many time steps should be simulated? `1e3` by default.
#' @param noise A number indicating the standard deviation of the Gaussian noise
#' added to each time step. `0` by default (no noise).
#'
#' @param upper_lim,lower_lim The upper and lower limit for the simulation. Once the simulated value is out of the limits, it will be taken back to avoid instability of the simulation. Both should either be a single number or a numeric vector with the same length as the number of variables in the model. `Inf` and `-Inf` by default, which means no limits.
#' @return A `tibble` with the simulated time series.
#' @export
sim_NVAR <- function(model, init = NULL, length = 1e3, noise = 0,
                     upper_lim = Inf, lower_lim = -Inf) {
  if (is.null(init)) {
    data <- model$data
  } else {
    data <- init[, model$parameters$vars]
  }
  sim_start <- nrow(data) + 1
  data_new <- matrix(nrow = length, ncol = length(model$parameters$vars)) %>%
    `colnames<-`(model$parameters$vars) %>%
    tibble::as_tibble()
  data <- dplyr::bind_rows(data, data_new)

  for (i in sim_start:nrow(data)) {
    feature_vec <- purrr::map_dbl(model$expressions, rlang::eval_tidy, data = data, rlang::env(t = i))
    result <- t(model$W_out %*% feature_vec) + stats::rnorm(length(model$parameters$vars), sd = noise)
    result <- pmax(pmin(result, upper_lim), lower_lim)
    data[i, ] <- result
  }

  attr(data, "sim_start") <- sim_start
  return(data)
}
