# The cringyness example of "Wait, what are you saying, exactly?"
# Numbers in the comments are the ones printed in the paper.

m <- cringe_models()

test_that("Model 4: 5 + 2 non-redundant inequalities, 7 vertices (Figure 2, Table 1)", {
  i <- fairy_info(m$M4)
  expect_equal(i$inequalities, 7)
  expect_equal(i$equalities, 0)
  expect_equal(i$vertices, 7)
  # 0 <= p3 <= p2 <= p1 <= p4 <= p5 <= p6 <= 1 is the whole model
  expect_equal(fairy_volume(m$M4)$max_bf, 720)
})

test_that("Table 1: the seven vertices of Model 4", {
  table1 <- rbind(
    c("all",  "all",  "all",  "all",  "all",  "all"),   # V4
    c("all",  "all",  "none", "all",  "all",  "all"),   # V5
    c("all",  "none", "none", "all",  "all",  "all"),   # V1
    c("none", "none", "none", "all",  "all",  "all"),   # V6
    c("none", "none", "none", "none", "all",  "all"),   # V3
    c("none", "none", "none", "none", "none", "all"),   # V2
    c("none", "none", "none", "none", "none", "none")   # V7
  )
  expect_true(same_rows(fairy_vertices(m$M4), words_to_num(table1)))
})

test_that("Model 9 is Model 4 with 'all' and 'none' swapped", {
  v4 <- fairy_vertices(m$M4)
  expect_true(same_rows(fairy_vertices(m$M9), 1 - v4))
  expect_equal(fairy_info(m$M9)$vertices, 7)
})

test_that("Model 5 (no effect): two inequalities, five equalities, two extreme cases", {
  i <- fairy_info(m$M5)
  expect_equal(c(i$inequalities, i$equalities, i$vertices), c(2, 5, 2))
  expect_equal(i$dim, 1)
  expect_equal(fairy_max_bf(m$M5), Inf)
})

test_that("Table 2: the twelve vertices of Model 6", {
  third <- "1/3"; two_thirds <- "2/3"; half <- "1/2"
  table2 <- rbind(
    c("none", "none", "none", "none", "none", "none"),       # V1
    c("all", "all", "none", two_thirds, two_thirds, two_thirds), # V2
    c("all", "none", "none", "none", half, half),            # V3
    c("all", "none", "none", third, third, third),           # V4
    c("all", half, half, "none", "all", "all"),              # V5
    c("all", "all", "none", half, half, "all"),              # V6
    c(half, half, "none", "none", "none", "all"),            # V7
    c(two_thirds, two_thirds, two_thirds, "none", "all", "all"), # V8
    c(third, third, third, "none", "none", "all"),           # V9
    c("all", "none", "none", "none", "none", "all"),         # V10
    c("all", "all", "none", "none", "all", "all"),           # V11
    c("all", "all", "all", "all", "all", "all")              # V12
  )
  expect_equal(fairy_info(m$M6)$vertices, 12)
  expect_true(same_rows(fairy_vertices(m$M6), words_to_num(table2)))
  expect_equal(fairy_dim(m$M6), 5)
})

test_that("Model 10 (mixture of 4 and 9): 12 vertices, 64 constraints of which 12 are ranges", {
  i <- fairy_info(m$M10)
  expect_equal(i$vertices, 12)
  expect_equal(i$inequalities, 64)
  expect_equal(i$equalities, 0)
  hn <- rcdd::q2d(m$M10)
  # range constraints involve a single probability
  expect_equal(sum(rowSums(hn[, -(1:2), drop = FALSE] != 0) == 1), 12)
  # all vertices have 0/1 coordinates; 12 of the 2^6 = 64 corners of the cube
  v <- fairy_vertices(m$M10)
  expect_true(all(v %in% c(0, 1)))
  # the vertices are the union of the two components' vertices
  expect_true(same_rows(v, unique(rbind(fairy_vertices(m$M4), fairy_vertices(m$M9)))))
})

test_that("Vertex V1 of Table 1 sits on 26 of the 52 order constraints and on all 6 relevant range constraints", {
  hn <- rcdd::q2d(m$M10)
  v1 <- c(1, 0, 0, 1, 1, 1)                    # p1 = p4 = p5 = p6 = 1, p2 = p3 = 0
  slack <- hn[, 2] + as.vector(hn[, -(1:2), drop = FALSE] %*% v1)
  expect_true(all(slack > -1e-9))              # a vertex of Model 10 satisfies every constraint
  single <- rowSums(hn[, -(1:2), drop = FALSE] != 0) == 1
  expect_equal(sum(abs(slack[!single]) < 1e-9), 26)
  expect_equal(sum(abs(slack[single]) < 1e-9), 6)
  expect_equal(sum(abs(slack) < 1e-9), 26 + 6)
})

test_that("Table 3: volumes and upper bounds on the Bayes factor", {
  expect_equal(fairy_volume(m$M1)$volume, 1/6)
  expect_equal(fairy_max_bf(m$M1), 6)
  expect_equal(fairy_max_bf(m$M2), 36)
  expect_equal(fairy_max_bf(m$M3), 72)
  expect_equal(fairy_max_bf(m$M4), 720)
  expect_equal(fairy_max_bf(m$M9), 720)
  expect_equal(fairy_max_bf(m$M6), Inf)
  expect_equal(fairy_max_bf(m$M10), 22.5, tolerance = 1e-6)    # "about 23"
  expect_equal(fairy_volume(m$M10)$volume, 0.04, tolerance = 0.12)  # "approximately 0.04"
  # Models 7 and 8 use an approximate equality with tolerance 0.05
  expect_equal(fairy_volume(m$M7)$volume, 0.005, tolerance = 0.15)
  expect_equal(fairy_volume(m$M8)$volume, 1e-6, tolerance = 1)   # printed as 10^-6
})

test_that("Table 3: relationships between the models", {
  cmp <- fairy_compare(m)
  rel <- cmp$relation
  # nested pairs shown in the paper (row model nested in column model)
  expect_equal(rel["M2", "M1"], "subset")
  expect_equal(rel["M4", "M3"], "subset")
  expect_equal(rel["M5", "M1"], "subset")
  expect_equal(rel["M6", "M3"], "subset")
  expect_equal(rel["M6", "M5"], "superset")
  expect_equal(rel["M10", "M4"], "superset")
  expect_equal(rel["M10", "M9"], "superset")
  # dimension of overlaps
  expect_equal(rel["M3", "M1"], "6D")
  expect_equal(rel["M3", "M2"], "4D")
  expect_equal(rel["M6", "M2"], "3D")
  expect_equal(rel["M6", "M4"], "1D")
  expect_equal(rel["M9", "M3"], "1D")
  expect_equal(rel["M9", "M4"], "1D")
  expect_equal(rel["M9", "M7"], "2D")
  # each 1D overlap is Model 5, whose V-representation is 2 points
  # overlap volumes above the diagonal
  expect_equal(cmp$volume["M1", "M2"], 1/36, tolerance = 0.2)
  expect_equal(cmp$volume["M1", "M3"], 0.003, tolerance = 0.15)
  expect_equal(cmp$volume["M3", "M3"], 0.014, tolerance = 0.05)
  expect_equal(cmp$volume["M9", "M10"], 1/720, tolerance = 1e-6)   # Model 9 is nested in Model 10
  expect_equal(unname(cmp$dim["M5"]), 1)
})

test_that("Figure 3: two rectangles, their overlap, and a line segment", {
  I   <- fairy_h("p1 < .5; p2 > .25; p2 < .75", n_p = 2)
  II  <- fairy_h("p1 > .3; p1 < .7; p2 > .5", n_p = 2)
  III <- fairy_intersect(I, II)
  IV  <- fairy_h("p1 = .5; p2 > .25; p2 < .75", n_p = 2)   # a 1D polytope
  expect_equal(fairy_volume(I)$volume, 0.25);  expect_equal(fairy_max_bf(I), 4)
  expect_equal(fairy_volume(II)$volume, 0.2);  expect_equal(fairy_max_bf(II), 5)
  expect_equal(fairy_volume(III)$volume, 0.05); expect_equal(fairy_max_bf(III), 20)
  # two of the four vertices of III are new (not vertices of I or II)
  vI <- fairy_vertices(I); vII <- fairy_vertices(II); vIII <- fairy_vertices(III)
  expect_equal(nrow(vIII), 4)
  is_in <- function(v, set) apply(v, 1, function(r) any(colSums(abs(t(set) - r)) < 1e-9))
  expect_equal(sum(!is_in(vIII, vI) & !is_in(vIII, vII)), 2)
  expect_equal(fairy_dim(IV), 1)
  expect_equal(fairy_max_bf(IV), Inf)
})

test_that("Table 4: Model 4 across three labs", {
  m4 <- m$M4
  ident <- fairy_info(fairy_replicate(m4, 3, "identical"))
  expect_equal(c(ident$dim, ident$vertices, ident$inequalities, ident$equalities), c(6, 7, 7, 12))
  expect_equal(ident$max_bf, Inf)

  joint_h <- fairy_replicate(m4, 3, "joint")
  joint <- fairy_info(joint_h)
  expect_equal(c(joint$dim, joint$vertices, joint$inequalities, joint$equalities), c(18, 343, 21, 0))
  expect_equal(joint$max_bf, 720^3)               # "7203 = 4 x 10^8"

  subs_h <- fairy_replicate(m4, 3, "substitutable")
  subs <- fairy_info(subs_h, volume = FALSE)
  expect_equal(c(subs$dim, subs$vertices, subs$inequalities, subs$equalities), c(18, 43, 51, 0))
  # 1.37 x 10^11 = 18! / (3!)^6: the 18 values fall into six ordered groups of three
  exact_bf <- factorial(18) / factorial(3)^6
  expect_equal(exact_bf, 1.37e11, tolerance = 0.01)
  skip_if_not_installed("volesti")
  est <- fairy_max_bf(subs_h, method = "volesti")   # a stochastic estimate
  expect_equal(est, exact_bf, tolerance = 0.6)
})

test_that("Table 4: Model 9 across three labs behaves like Model 4", {
  m9 <- m$M9
  joint <- fairy_info(fairy_replicate(m9, 3, "joint"))
  expect_equal(c(joint$vertices, joint$inequalities), c(343, 21))
  subs <- fairy_info(fairy_replicate(m9, 3, "substitutable"), volume = FALSE)
  expect_equal(c(subs$vertices, subs$inequalities), c(43, 51))
})

test_that("Table 4: Model 10 across three labs", {
  m10 <- m$M10
  ident <- fairy_info(fairy_replicate(m10, 3, "identical"))
  expect_equal(c(ident$dim, ident$vertices, ident$inequalities, ident$equalities), c(6, 12, 64, 12))

  skip_on_cran()
  joint <- fairy_info(fairy_replicate(m10, 3, "joint"))
  expect_equal(c(joint$dim, joint$vertices, joint$inequalities, joint$equalities), c(18, 1728, 192, 0))
  expect_equal(joint$max_bf, fairy_max_bf(m10)^3)    # "23^3 = 10^4"
  expect_equal(joint$max_bf, 1e4, tolerance = 0.2)

  # substitutable replication: 4,032 constraints (checked here before the
  # redundancy removal, which takes minutes in 18 dimensions)
  subs <- fairy_replicate(m10, 3, "substitutable", reduce = FALSE)
  expect_equal(nrow(subs), 4032)
})

test_that("a joint model's volume is the single-lab volume to the power r", {
  expect_equal(fairy_volume(fairy_replicate(m$M4, 3, "joint"))$volume, (1/720)^3)
  expect_equal(fairy_volume(fairy_replicate("p1 > p2", 2, "joint", n_p = 2))$volume, 0.25)
})
