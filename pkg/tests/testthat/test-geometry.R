test_that("volume of simple orderings is 1/k!", {
  expect_equal(fairy_volume("p1 > p2 > p3")$volume, 1/6)
  expect_equal(fairy_volume("p1 > p2 > p3 > p4")$volume, 1/24)
  expect_equal(fairy_max_bf("p1 > p2 > p3 > p4 > p5"), 120)
})

test_that("constraints on separate probabilities multiply", {
  expect_equal(fairy_volume("p1 < .5; p2 > .25; p2 < .75")$volume, 0.25)
  expect_equal(fairy_volume("p1 > p2; p3 > p4", n_p = 4)$volume, 1/4)
})

test_that("models with an equality have dimension below n_p, volume zero and no bound", {
  expect_equal(fairy_dim("p1 = p2; p2 > p3"), 2)
  v <- fairy_volume("p1 = p2; p2 > p3")
  expect_equal(v$volume, 0)
  expect_equal(v$max_bf, Inf)
  expect_equal(v$method, "lower-dimensional")
})

test_that("Monte Carlo agrees with the exact volume", {
  set.seed(1)
  mc <- fairy_volume("p1 > p2 > p3", method = "mc", n_samples = 4e5)
  expect_equal(mc$volume, 1/6, tolerance = 0.02)
  expect_false(is.na(mc$se))
})

test_that("<=, >=, == and the unicode symbols mean the same as < and >", {
  a <- fairy_h("p1 > p2 > p3", n_p = 3)
  expect_equal(fairy_h("p1 >= p2 >= p3", n_p = 3), a)
  expect_true(same_rows(rcdd::q2d(fairy_h("p3 <= p2 <= p1", n_p = 3)), rcdd::q2d(a)))
  expect_equal(fairy_h("p1 ≥ p2 ≥ p3", n_p = 3), a)
  expect_equal(nrow(fairy_h("p1 == p2", n_p = 2)), nrow(fairy_h("p1 = p2", n_p = 2)))
})

test_that("fairy_words uses 'all' and 'none' and fractions", {
  w <- fairy_words("p1 > p2", n_p = 2)
  expect_s3_class(w, "fairy_words")
  expect_setequal(unlist(w), c("all", "none"))
  w6 <- fairy_words(cringe_models()$M6)
  expect_true(any(unlist(w6) == "2/3"))
  expect_equal(nrow(w6), 12)
})

test_that("intersection and mixture", {
  i <- fairy_intersect("p1 > p2", "p2 > p3", n_p = 3)
  expect_equal(fairy_volume(i)$volume, 1/6)
  expect_equal(fairy_dim(fairy_intersect("p1 > .8", "p1 < .2", n_p = 1)), -1)
  mx <- fairy_mixture("p1 > .8; p2 < .2", "p1 < .2; p2 > .8", n_p = 2)
  expect_equal(fairy_info(mx)$vertices, 6)    # the hull of two opposite rectangles is a hexagon
  expect_true(fairy_nested("p1 > .8; p2 < .2", mx, n_p = 2))
})

test_that("nesting is directional", {
  expect_true(fairy_nested("p1 > p2 > p3", "p1 > p2", n_p = 3))
  expect_false(fairy_nested("p1 > p2", "p1 > p2 > p3", n_p = 3))
})

test_that("fairy_compare returns the table of the paper's layout", {
  cmp <- fairy_compare(list(A = "p1 > p2 > p3", B = "p1 > p2", C = "p3 > p1"), n_p = 3)
  expect_s3_class(cmp, "fairy_compare")
  expect_equal(cmp$relation["B", "A"], "superset")   # lower triangle: row B contains column A
  expect_equal(cmp$relation["C", "A"], "1D")
  expect_equal(unname(cmp$max_bf), c(6, 2, 2))
  expect_equal(cmp$volume["A", "B"], 1/6)
  expect_output(print(cmp), "maxBF")
})

test_that("replication: identical, joint and substitutable on a small model", {
  h <- fairy_h("p1 > p2", n_p = 2)
  i <- fairy_replicate(h, 2, "identical")
  expect_equal(fairy_dim(i), 2)
  j <- fairy_replicate(h, 2, "joint")
  expect_equal(fairy_volume(j)$volume, 0.25)
  s <- fairy_replicate(h, 2, "substitutable")
  # every lab's p1 above every lab's p2: 4 order constraints
  expect_equal(fairy_info(s)$inequalities, 4 + 4)
  expect_equal(fairy_max_bf(s), 1 / (factorial(2)^2 / factorial(4)))
  expect_error(fairy_replicate(fairy_h("p1 = p2", n_p = 2), 2, "substitutable"), "equality")
})

test_that("fairy() reports volume and the maximal Bayes factor", {
  res <- fairy("p1 > p2 > p3 > p4", n_p = 4)
  expect_equal(res$parsimony, 1/24)
  expect_equal(res$max_bf, 24)
  expect_output(print(res), "Max. BF")
})
