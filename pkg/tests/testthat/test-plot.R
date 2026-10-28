test_that("fairy_plot() builds a plotly figure for 3D, 2D and projected models", {
  skip_if_not_installed("plotly")
  m <- list(A = fairy_h("p1 > p2 > p3", n_p = 3), B = fairy_h("p1 > p3", n_p = 3),
            C = fairy_h("p1 = p2; p2 > p3", n_p = 3), D = fairy_h("p1 = p2 = p3", n_p = 3))
  expect_s3_class(fairy_plot(m), "plotly")
  expect_s3_class(fairy_plot(fairy_h("p1 > p2", n_p = 2)), "plotly")
  h4 <- fairy_h("p1 > p2 > p3 > p4", n_p = 4)
  expect_s3_class(fairy_plot(h4, dims = c(1, 2, 3)), "plotly")
  expect_s3_class(fairy_plot(h4, dims = c(1, 4)), "plotly")
  expect_error(fairy_plot(h4, dims = 1), "two or three")
})
