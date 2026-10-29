test_that("QTEST and multinomineq writers accept strings and H-representations", {
  h1 <- fairy_h("p1 > p2 > p3", n_p = 3)
  h2 <- fairy_h("p1 < p2; p2 = p3", n_p = 3)
  mods <- list(m1 = h1, m2 = h2, inters = fairy_intersect(h1, h2), txt = "p1 > .2")
  d <- tempfile(); dir.create(d)
  q <- fairy_write_qtest(mods, n_p = 3, file = file.path(d, "q.tar"))
  m <- fairy_write_multinomineq(mods, n_p = 3, file = file.path(d, "m.tar"))
  qf <- utils::untar(q, list = TRUE); mf <- utils::untar(m, list = TRUE)
  expect_true(all(c("m1.txt", "m2.txt", "m2_substitutions.txt", "txt.txt") %in% qf))
  expect_true(all(c("A_m1.csv", "b_m1.csv", "A_txt.csv") %in% mf))
  utils::untar(q, exdir = file.path(d, "q")); utils::untar(m, exdir = file.path(d, "m"))
  # m2 has the equality p2 = p3: one probability is eliminated, two are kept
  note <- readLines(file.path(d, "q", "m2_substitutions.txt"))
  expect_true(any(grepl("Kept parameters", note)))
  expect_equal(strsplit(readLines(file.path(d, "q", "m2.txt"))[1], " ")[[1]][2], "2")
  # multinomineq: an equality is written in both directions (3 inequalities + 2 for p2 = p3)
  expect_equal(length(readLines(file.path(d, "m", "b_m2.csv"))), 5)
})

test_that("fairy_write_qtest(order = ) puts the probabilities of a model in the chosen order", {
  d <- tempfile(); dir.create(d)
  a <- fairy_write_qtest(c(m = "p1 > p2 > p3"), n_p = 3, file = file.path(d, "a.tar"))
  b <- fairy_write_qtest(c(m = "p1 > p2 > p3"), n_p = 3, file = file.path(d, "b.tar"), order = list(m = c(3, 1, 2)))
  utils::untar(a, exdir = file.path(d, "a")); utils::untar(b, exdir = file.path(d, "b"))
  ta <- readLines(file.path(d, "a", "m.txt")); tb <- readLines(file.path(d, "b", "m.txt"))
  expect_false(identical(ta, tb))
  # the same rows, only the columns are in another order: the first column of b is the third one of a
  rows <- function(t) lapply(t[nzchar(t)][-1], function(l) strsplit(trimws(l), "\\s+")[[1]])
  ra <- rows(ta)[1:2]; rb <- rows(tb)[1:2]
  expect_equal(length(ra[[1]]), length(rb[[1]]))
  expect_error(fairy_write_qtest(c(m = "p1 > p2 > p3"), n_p = 3, file = file.path(d, "c.tar"), order = list(m = c(1, 1, 2))), "permutation")
})

test_that("fairy_write_multinomineq(order = ) reorders the columns of A", {
  d <- tempfile(); dir.create(d)
  a <- fairy_write_multinomineq(c(m = "p1 > p2 > p3"), n_p = 3, file = file.path(d, "a.tar"))
  b <- fairy_write_multinomineq(c(m = "p1 > p2 > p3"), n_p = 3, file = file.path(d, "b.tar"), order = list(m = c(3, 1, 2)))
  utils::untar(a, exdir = file.path(d, "a")); utils::untar(b, exdir = file.path(d, "b"))
  Aa <- utils::read.csv(file.path(d, "a", "A_m.csv"), header = TRUE, check.names = FALSE)
  Ab <- utils::read.csv(file.path(d, "b", "A_m.csv"), header = TRUE, check.names = FALSE)
  # column j of b is column order[j] of a, and the header follows
  expect_equal(unname(as.matrix(Ab)), unname(as.matrix(Aa)[, c(3, 1, 2)]))
  expect_equal(names(Ab), names(Aa)[c(3, 1, 2)])
})
