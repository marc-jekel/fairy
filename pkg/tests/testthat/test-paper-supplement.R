# The Heuristic-Systematic Model case study (online supplement)
hsm <- hsm_models()
lab4 <- c("bW", "bS", "sW", "sS")
lab6 <- c("iW", "nW", "cW", "iS", "nS", "cS")

test_that("Theoretical Predictions 1 and 2: nine extreme cases each, a quarter of the cube", {
  for (nm in c("pred1", "pred2")) {
    i <- fairy_info(hsm[[nm]])
    expect_equal(i$vertices, 9)
    expect_equal(i$volume, 1/4)
    expect_equal(i$max_bf, 4)
  }
})

test_that("Table A1: extreme cases of Prediction 1", {
  tA1 <- rbind(                                            # big,weak  big,strong  small,weak  small,strong
    c("all", "all", "none", "none"), c("none", "all", "none", "none"), c("none", "none", "none", "none"),
    c("none", "none", "none", "all"), c("none", "none", "all", "all"), c("none", "all", "none", "all"),
    c("none", "all", "all", "all"), c("all", "all", "none", "all"), c("all", "all", "all", "all"))
  expect_true(same_rows(fairy_vertices(hsm$pred1), words_to_num(tA1)))
})

test_that("Table A2: extreme cases of Prediction 2", {
  tA2 <- rbind(
    c("all", "none", "all", "none"), c("none", "none", "all", "none"), c("none", "none", "none", "none"),
    c("none", "none", "none", "all"), c("none", "all", "none", "all"), c("none", "none", "all", "all"),
    c("none", "all", "all", "all"), c("all", "none", "all", "all"), c("all", "all", "all", "all"))
  expect_true(same_rows(fairy_vertices(hsm$pred2), words_to_num(tA2)))
})

test_that("Table A3: Prediction 3 has six extreme cases, shared by Tables A1 and A2", {
  tA3 <- rbind(
    c("none", "none", "none", "all"), c("none", "all", "none", "all"), c("none", "all", "all", "all"),
    c("none", "none", "all", "all"), c("none", "none", "none", "none"), c("all", "all", "all", "all"))
  v3 <- fairy_vertices(hsm$pred3)
  expect_true(same_rows(v3, words_to_num(tA3)))
  expect_equal(fairy_volume(hsm$pred3)$volume, 1/12)
  expect_equal(fairy_max_bf(hsm$pred3), 12)
  # no new vertex: all six are vertices of both Prediction 1 and Prediction 2
  in_set <- function(v, set) apply(v, 1, function(r) any(colSums(abs(t(set) - r)) < 1e-9))
  expect_true(all(in_set(v3, fairy_vertices(hsm$pred1))))
  expect_true(all(in_set(v3, fairy_vertices(hsm$pred2))))
  # predictions 1 and 2 overlap in Prediction 3
  expect_true(fairy_nested(hsm$pred3, hsm$pred1))
  expect_true(fairy_nested(hsm$pred3, hsm$pred2))
  i12 <- fairy_intersect(hsm$pred1, hsm$pred2)
  expect_equal(fairy_volume(i12)$volume, 1/12)
})

test_that("Two populations: joint model has bound 12^2 = 144, substitutable model 420", {
  expect_equal(fairy_max_bf(fairy_replicate(hsm$pred3, 2, "joint")), 144)
  expect_equal(fairy_max_bf(fairy_replicate(hsm$pred3, 2, "substitutable")), 420, tolerance = 1e-6)
})

test_that("A 3-by-3 design: 14 constraints for nine probabilities, bound about 8,600", {
  i <- fairy_info(hsm$pred6)
  expect_equal(i$inequalities, 12 + 2)     # twelve order constraints and the two range constraints
  expect_equal(i$max_bf, 8640, tolerance = 0.01)    # "approximately 8,600"
  expect_gt(i$max_bf / 144, 59)                     # "almost 60 times larger than 144"
})

test_that("Table A4 (top): the six extreme cases of the Override Model", {
  tA4 <- rbind(                                   # inc W, no W, con W, inc S, no S, con S
    c("none", "none", "none", "none", "none", "all"), c("none", "none", "all", "none", "none", "all"),
    c("none", "none", "all", "all", "all", "all"), c("none", "none", "none", "all", "all", "all"),
    c("none", "none", "none", "none", "none", "none"), c("all", "all", "all", "all", "all", "all"))
  expect_true(same_rows(fairy_vertices(hsm$override), words_to_num(tA4)))
  expect_equal(fairy_max_bf(hsm$override), Inf)        # equality constraints: volume zero
  expect_equal(fairy_dim(hsm$override), 4)
})

test_that("Trade-off Model: 10 vertices and a bound of about 144, Override is nested in it", {
  i <- fairy_info(hsm$tradeoff)
  expect_equal(i$vertices, 10)
  expect_equal(i$max_bf, 144)
  expect_true(fairy_nested(hsm$override, hsm$tradeoff))
  expect_false(fairy_nested(hsm$tradeoff, hsm$override))
})

test_that("Take-the-best (Table A4 bottom) violates the Override Model", {
  expect_true(same_rows(fairy_vertices(hsm$ttb),
                        words_to_num(rbind(c("none", "all", "all", "none", "all", "all")))))
  expect_false(fairy_nested(hsm$ttb, hsm$override))
})

test_that("Mixture of Override and TTB: one added vertex, one equality, volume zero", {
  mix <- hsm$override_ttb_mixture
  i <- fairy_info(mix)
  expect_equal(i$vertices, 7)
  expect_equal(i$equalities, 1)
  # equation (10): one equality, five order constraints, two range constraints
  expect_equal(i$inequalities, 5 + 2)
  expect_equal(fairy_max_bf(mix), Inf)
  expect_true(fairy_nested(hsm$override, mix))
  expect_true(fairy_nested(hsm$ttb, mix))
  # the added vertex is "none, all, all, none, all, all"
  expect_true(same_rows(fairy_vertices(mix),
                        rbind(fairy_vertices(hsm$override), c(0, 1, 1, 0, 1, 1))))
})
