library(fairy)
library(rcdd)

test_that("fairy_h takes a single model string", {
  h <- fairy_h("p1 > p2 > p3", n_p = 3)
  expect_true(is.matrix(h))
  expect_equal(ncol(h), 5)
  expect_true(nrow(h) >= 2)
  expect_true(all(h[, 1] %in% c("0", "1")))
})

test_that("fairy_h rejects a vector", {
  expect_error(fairy_h(c("p1 > p2", "p2 > p3"), n_p = 3), "single model string")
})

test_that("semicolons within a model string work", {
  h1 <- fairy_h("p1 > p2; p2 > p3", n_p = 3)
  h2 <- fairy_h("p1 > p2 > p3",     n_p = 3)
  expect_equal(nrow(h1), nrow(h2))
})

test_that("fairy_v returns vertices satisfying the constraints", {
  h     <- fairy_h("p1 > p2 > p3", n_p = 3)
  v     <- fairy_v(h)
  v_num <- q2d(v)
  verts <- v_num[v_num[, 1] == 0 & v_num[, 2] == 1, 3:5, drop = FALSE]
  expect_true(nrow(verts) > 0)
  expect_true(all(verts[, 1] >= verts[, 2] - 1e-9))
  expect_true(all(verts[, 2] >= verts[, 3] - 1e-9))
})

test_that("parsimony of full ordering is close to 1/6", {
  h    <- fairy_h("p1 > p2 > p3", n_p = 3)
  pars <- fairy_parsimony(h, n_p = 3, n_samples = 2e5)
  expect_equal(pars$parsimony, 1/6, tolerance = 0.01)
})

test_that("fairy() with single model returns a fairy object", {
  res <- fairy("p1 > p2 > p3", n_p = 3)
  expect_s3_class(res, "fairy")
  expect_false(is.null(res$h))
  expect_false(is.null(res$parsimony))
})

test_that("fairy() with multiple models returns a named list of fairy objects", {
  res <- fairy(c(H1 = "p1 > p2 > p3", H2 = "p1 > p2"), n_p = 3)
  expect_type(res, "list")
  expect_equal(names(res), c("H1", "H2"))
  expect_s3_class(res$H1, "fairy")
  expect_s3_class(res$H2, "fairy")
})

test_that("parsimony of H2 (p1>p2 only) is larger than H1 (p1>p2>p3)", {
  res <- fairy(c(H1 = "p1 > p2 > p3", H2 = "p1 > p2"), n_p = 3, n_samples = 2e5)
  expect_gt(res$H2$parsimony, res$H1$parsimony)
})

test_that("equality constraints produce an equality row in H-rep", {
  h <- fairy_h("p1 = p2; p1 > p3", n_p = 3)
  expect_true(any(h[, 1] == "1"))
})

test_that("unnamed models get auto-names M1, M2, ...", {
  res <- fairy(c("p1 > p2", "p2 > p3"), n_p = 3)
  expect_equal(names(res), c("M1", "M2"))
})
