test_that("fairy_volume(method = 'auto') is exact when it finishes in time", {
  v <- fairy_volume("p1 > p2 > p3", n_p = 3, method = "auto")
  expect_equal(v$volume, 1/6)
  expect_identical(v$method, "exact")
  expect_equal(fairy_max_bf("p1 > p2 > p3", n_p = 3, method = "auto"), 6)
})

test_that("fairy_volume(method = 'auto') falls back to volesti after the time limit", {
  skip_if_not_installed("callr"); skip_if_not_installed("volesti")
  h <- fairy_h(paste0("p", 1:8, " < 0.2", collapse = "; "), n_p = 8)
  v <- fairy_volume(h, method = "auto", time_limit = 0.001)
  expect_match(v$method, "volesti")
  expect_match(v$method, "too long")
  expect_gt(v$volume, 0)
})

test_that("the multinomineq archive carries the analysis script, the data files and the settings", {
  d <- tempfile(); dir.create(d)
  f <- fairy_write_multinomineq(c(H1 = "p1 > p2 > p3", H2 = "p1 > p2"), n_p = 3, file = file.path(d, "a.tar"),
                                data = data.frame(probability = c("p1", "p2", "p3"), n = c(30, 30, 45), frequency = c(21, 12, 9)),
                                settings = list(bf_samples = 20000, posterior_samples = 2000, prior = c(2, 3), seed = 99))
  x <- file.path(d, "x"); dir.create(x); utils::untar(f, exdir = x)
  fl <- list.files(x)
  expect_true(all(c("A_H1.csv", "b_H1.csv", "A_H2.csv", "analysis_multinomineq.R", "data_choices.csv") %in% fl))
  dat <- utils::read.csv(file.path(x, "data_choices.csv"))
  expect_equal(dat$n, c(30, 30, 45)); expect_equal(dat$frequency, c(21, 12, 9))
  sc <- readLines(file.path(x, "analysis_multinomineq.R"))
  expect_true("M_bf <- 20000" %in% sc); expect_true("M_post <- 2000" %in% sc)
  expect_true("prior <- c(2, 3)" %in% sc); expect_true(any(startsWith(sc, "seed <- 99"))); expect_true("final <- TRUE" %in% sc)
})

test_that("the data stay with the probability when the order changes, and models can have their own data", {
  d <- tempfile(); dir.create(d)
  f <- fairy_write_multinomineq(c(H1 = "p1 > p2 > p3"), n_p = 3, file = file.path(d, "o.tar"), order = list(H1 = c(3, 1, 2)),
                                data = data.frame(probability = c("p1", "p2", "p3"), n = c(10, 20, 30), frequency = c(1, 2, 3)))
  x <- file.path(d, "x"); dir.create(x); utils::untar(f, exdir = x)
  dat <- utils::read.csv(file.path(x, "data_choices.csv"))
  expect_equal(gsub("[_{}]", "", dat$probability), c("p3", "p1", "p2")); expect_equal(dat$n, c(30, 10, 20))
  f2 <- fairy_write_multinomineq(list(H1 = "p1 > p2", H2 = "p1 > p2 > p3"), n_p = 3, file = file.path(d, "p.tar"),
                                 data = list(H2 = data.frame(probability = c("p1", "p2", "p3"), n = 5, frequency = c(4, 3, 2))))
  y <- file.path(d, "y"); dir.create(y); utils::untar(f2, exdir = y)
  expect_true(all(c("data_model_H2.csv", "data_choices.csv") %in% list.files(y)))
  expect_equal(utils::read.csv(file.path(y, "data_model_H2.csv"))$frequency, c(4, 3, 2))
})

test_that("analysis = FALSE writes only the model files; bad settings are refused", {
  d <- tempfile(); dir.create(d)
  f <- fairy_write_multinomineq(c(H1 = "p1 > p2"), n_p = 2, file = file.path(d, "n.tar"), analysis = FALSE)
  x <- file.path(d, "x"); dir.create(x); utils::untar(f, exdir = x)
  expect_setequal(list.files(x), c("A_H1.csv", "b_H1.csv"))
  expect_error(fairy_multinomineq_script(prior = c(0, 1)), "positive")
  expect_error(fairy_multinomineq_script(bf_samples = 50), "100 or more")
})

test_that("fairy_analyze_multinomineq runs the analysis and returns the results", {
  skip_on_cran(); skip_if_not_installed("multinomineq"); skip_if_not_installed("coda")
  r <- fairy_analyze_multinomineq(c(H1 = "p1 > p2 > p3", H2 = "p1 > p2"), n_p = 3,
         data = data.frame(probability = c("p1", "p2", "p3"), n = c(30, 30, 30), frequency = c(21, 12, 9)),
         settings = list(posterior_samples = 200, bf_samples = 500, seed = 1))
  expect_s3_class(r$results, "data.frame")
  expect_true(all(c("H1", "H2") %in% unlist(r$results[, 1])))
  expect_s3_class(r$posterior, "data.frame")
})

test_that("fairy_compare_vertices finds shared and unique vertices", {
  r <- fairy_compare_vertices(c(A = "p1 > p2 > p3", B = "p1 > p2", C = "p3 > p1"), n_p = 3)
  s <- r$summary
  expect_equal(s$vertices, c(4, 6, 6))
  # A has the vertices (0,0,0), (1,0,0), (1,1,0), (1,1,1); B contains all four, C has (0,0,0) and (1,1,1) among others
  expect_equal(s$unique[s$model == "A"], 0)
  expect_true(all(r$vertices$shared_with[r$vertices$model == "A"] != ""))
  expect_output(print(r), "Vertices shared")
  # a mixture is not compared with its own components
  mx <- fairy_mixture("p1 > p2", "p2 > p1", n_p = 2)
  r2 <- fairy_compare_vertices(list(A = "p1 > p2", B = "p2 > p1", C = "p1 > 0.5", MIX = mx), n_p = 2,
                               derived = "MIX", components = list(MIX = c("A", "B")))
  expect_equal(r2$summary$compared_with[r2$summary$model == "MIX"], "C")
  expect_equal(r2$summary$compared_with[r2$summary$model == "A"], "B, C")
})
