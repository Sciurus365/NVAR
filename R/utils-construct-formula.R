construct_formula <- function(vars, expressions) {
  rhs <- expressions %>%
    lapply(rlang::expr_deparse) %>%
    c() %>%
    paste0(collapse = "+") %>%
    rlang::parse_expr()

  lapply(vars,
         function(x){
           x <- rlang::parse_expr(x)
           rlang::expr(!!x ~ !!rhs)
         })
  # return a list of global models for each variable
}
