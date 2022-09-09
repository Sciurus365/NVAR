test_that("`make_expressions` works", {
  expect_equal(
    make_expressions(c("x", "y"), s = 3, k = 3, p = 2),
    rlang::exprs(
      x[t - 0], x[t - 3], x[t - 6], y[t - 0], y[t - 3], y[t -
        6], x[t - 0]^2, x[t - 0] * x[t - 3], x[t - 0] * x[t - 6],
      x[t - 0] * y[t - 0], x[t - 0] * y[t - 3], x[t - 0] * y[t -
        6], x[t - 3]^2, x[t - 3] * x[t - 6], x[t - 3] * y[t -
        0], x[t - 3] * y[t - 3], x[t - 3] * y[t - 6], x[t - 6]^2,
      x[t - 6] * y[t - 0], x[t - 6] * y[t - 3], x[t - 6] * y[t -
        6], y[t - 0]^2, y[t - 0] * y[t - 3], y[t - 0] * y[t -
        6], y[t - 3]^2, y[t - 3] * y[t - 6], y[t - 6]^2
    ) |> `names<-`(x = _, NULL)
  )
})
