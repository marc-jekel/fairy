same_model <- function(a, b) fairy_nested(a, b) && fairy_nested(b, a)

test_that("fairy_unpack reproduces fairy_replicate when all probabilities share one list", {
  m <- fairy_h("p1 > p2; p2 > p3", n_p = 3)
  for (ty in c("joint", "identical", "substitutable")) {
    a <- fairy_unpack(m, items = 2, type = ty)
    b <- fairy_replicate(m, r = 2, type = ty)
    expect_equal(colnames(a), colnames(b))
    expect_true(same_model(a, b), info = ty)
  }
})

test_that("average is implied by the stricter types and implies nothing about single items", {
  m <- fairy_h("p1 > p2; p2 > p3", n_p = 3)
  avg  <- fairy_unpack(m, items = 2, type = "average")
  subs <- fairy_unpack(m, items = 2, type = "substitutable")
  jnt  <- fairy_unpack(m, items = 2, type = "joint")
  expect_true(fairy_nested(subs, jnt))
  expect_true(fairy_nested(jnt, avg))
  expect_false(fairy_nested(avg, jnt))
})

test_that("identical with a tolerance sits between exact equality and joint", {
  m <- fairy_h("p1 > p2; p2 > p3", n_p = 3)
  ex  <- fairy_unpack(m, items = 2, type = "identical")
  tol <- fairy_unpack(m, items = 2, type = "identical", eps = 0.1)
  jnt <- fairy_unpack(m, items = 2, type = "joint")
  expect_true(fairy_nested(ex, tol))
  expect_true(fairy_nested(tol, jnt))
  expect_false(fairy_nested(tol, ex))
})

test_that("lists can differ in size and type", {
  h <- fairy_unpack("p3 > p1 + p2", items = c(2, 2, 3), lists = c("A", "A", "B"),
                    type = c(A = "substitutable", B = "average"), n_p = 3)
  expect_equal(ncol(h) - 2L, 7L)
  b <- colnames(fairy_h("p3 > p1 + p2", n_p = 3))[-(1:2)]
  expect_equal(colnames(h)[-(1:2)],
               paste0(c(b[1], b[2], b[1], b[2], b[3], b[3], b[3]), "^(", c(1, 1, 2, 2, 1, 2, 3), ")"))
  expect_error(fairy_unpack("p1 > p2", items = c(2, 3), lists = c("A", "A"), n_p = 2),
               "same number of items")
})

test_that("intersections and mixtures of unpacked models match the columns by name", {
  m <- fairy_h("p1 > p2; p2 > p3", n_p = 3)
  avg  <- fairy_unpack(m, items = 2, type = "average")
  subs <- fairy_unpack(m, items = 2, type = "substitutable")
  # the same items, grouped differently: same columns in another order
  subs_split <- fairy_unpack(m, items = 2, lists = c("A", "B", "C"), type = "substitutable")
  expect_false(identical(colnames(subs), colnames(subs_split)))
  expect_true(same_model(fairy_intersect(avg, subs), subs))
  expect_true(same_model(fairy_intersect(avg, subs_split), subs))
  expect_true(same_model(fairy_mixture(avg, subs_split), avg))
  expect_error(fairy_intersect(avg, fairy_unpack(m, items = 3, type = "average")),
               "same space")
})
