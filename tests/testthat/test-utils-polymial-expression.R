library(NVAR)
test_that("`make_expressions` works", {
  expect_equal(
    make_expressions(c("x", "y"), s = 3, k = 3, p = 2, constant = TRUE),
    rlang::exprs(
      1, x[t - 1], x[t - 4], x[t - 7], y[t - 1], y[t - 4], y[t -
        7], x[t - 1]^2, x[t - 1] * x[t - 4], x[t - 1] * x[t - 7],
      x[t - 1] * y[t - 1], x[t - 1] * y[t - 4], x[t - 1] * y[t -
        7], x[t - 4]^2, x[t - 4] * x[t - 7], x[t - 4] * y[t -
        1], x[t - 4] * y[t - 4], x[t - 4] * y[t - 7], x[t - 7]^2,
      x[t - 7] * y[t - 1], x[t - 7] * y[t - 4], x[t - 7] * y[t -
        7], y[t - 1]^2, y[t - 1] * y[t - 4], y[t - 1] * y[t -
        7], y[t - 4]^2, y[t - 4] * y[t - 7], y[t - 7]^2
    ) |> `names<-`(x = _, NULL)
  )
})
