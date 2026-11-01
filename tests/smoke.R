# Smoke test of the main flows in a real (headless) browser:
#   NOT_CRAN=true Rscript tests/smoke.R   (or tests/smoke.sh)
# Starts the app, types models, runs Go, opens the data window, downloads the
# multinomineq files and deletes a model. Stops with an error if something fails.
library(shinytest2)
library(testthat)

app_dir <- if (file.exists("app.R")) "." else ".."
app <- AppDriver$new(app_dir = app_dir, name = "fairy-smoke", load_timeout = 60000,
                     timeout = 60000, seed = 1)
on.exit(try(app$stop(), silent = TRUE), add = TRUE)
idle <- function(secs = 90) {
  app$wait_for_idle(duration = 1500, timeout = secs * 1000)
  invisible(NULL)
}
js <- function(code) app$run_js(code)
jsv <- function(code) app$get_js(code)
count_models <- "[].filter.call(document.querySelectorAll('textarea'), function(e) { return /^textin_relations_[0-9]+$/.test(e.id); }).length"

test_that("two models: type, Go, both appear", {
  idle()
  app$set_inputs(textin_relations_1 = "p1 > p2 > p3", wait_ = FALSE)
  Sys.sleep(1.5)
  js("document.getElementById('add_ie').click();")
  Sys.sleep(3)
  app$set_inputs(textin_relations_2 = "p1 < p2; p2 < p3", wait_ = FALSE)
  Sys.sleep(1.5)
  js("document.getElementById('go_v_h').click();")
  idle(120)
  Sys.sleep(2)
  n_models <- jsv(count_models)
  expect_equal(n_models, 2)
})

test_that("a hint appears for a wrong specification and goes away", {
  js("var t = document.getElementById('textin_relations_1'); t.value = 'p_{1} > p2'; t.dispatchEvent(new Event('input', {bubbles: true}));")
  Sys.sleep(1)
  expect_true(jsv("!!document.querySelector('.fairy-spec-hint')"))
  js("var t = document.getElementById('textin_relations_1'); t.value = 'p1 > p2 > p3'; t.dispatchEvent(new Event('input', {bubbles: true}));")
  Sys.sleep(1)
  expect_false(jsv("!!document.querySelector('.fairy-spec-hint')"))
})

test_that("multinomineq download contains script, data, A and b for both models", {
  js("document.getElementById('go_v_h').click();")
  idle(120)
  js("[].slice.call(document.querySelectorAll('[id*=tabs] a')).filter(function(a){return /H-representation/.test(a.textContent)})[0].click();")
  Sys.sleep(5)
  f <- app$get_download("d_h_multinomineq")
  files <- utils::untar(f, list = TRUE)
  expect_true("analysis_multinomineq.R" %in% files)
  expect_true(any(grepl("^data_choices", files)))
  expect_equal(sum(grepl("^A_", files)), 2)
  expect_equal(sum(grepl("^b_", files)), 2)
})

test_that("data window saves numbers into the data file", {
  js("document.getElementById('mn_data_open').click();")
  Sys.sleep(3)
  js("function setv(id, v) { var e = document.getElementById(id); e.value = v; e.dispatchEvent(new Event('input', {bubbles: true})); e.dispatchEvent(new Event('change', {bubbles: true})); } setv('mn_n_1_1', 40); setv('mn_f_1_1', 33);")
  Sys.sleep(1)
  js("document.getElementById('mn_data_save').click();")
  Sys.sleep(2)
  f <- app$get_download("d_h_multinomineq")
  d <- tempfile(); dir.create(d); utils::untar(f, exdir = d)
  dat <- utils::read.csv(list.files(d, "^data_choices", full.names = TRUE)[1])
  expect_equal(dat$n[1], 40)
  expect_equal(dat$frequency[1], 33)
})

test_that("the script runs on the downloaded files", {
  f <- app$get_download("d_h_multinomineq")
  d <- tempfile(); dir.create(d); utils::untar(f, exdir = d)
  txt <- sub("^M_bf <- .*", "M_bf <- 2000", readLines(file.path(d, "analysis_multinomineq.R")))
  txt <- sub("^final <- FALSE.*", "final <- FALSE", txt)
  writeLines(txt, file.path(d, "analysis_multinomineq.R"))
  setwd_old <- setwd(d); on.exit(setwd(setwd_old), add = TRUE)
  out <- suppressWarnings(system2("Rscript", "analysis_multinomineq.R", stdout = TRUE, stderr = TRUE))
  expect_true(file.exists("results.csv"))
  expect_true(file.exists("results.html"))
  res <- utils::read.csv("results.csv")
  expect_equal(nrow(res), 2)
  expect_true(all(is.finite(res$bf_0u)))
})

test_that("deleting a model leaves one", {
  # deleting happens on the Input tab (the boxes are only redrawn while it is visible)
  js("[].slice.call(document.querySelectorAll('[id*=tabs] a')).filter(function(a) { return /^Input$/.test(a.textContent.trim()); })[0].click();")
  Sys.sleep(3)
  js("Shiny.setInputValue('delete_model_idx', 2, {priority: 'event'});")
  Sys.sleep(8)
  idle(60)
  Sys.sleep(2)
  ids <- jsv("[].map.call(document.querySelectorAll('textarea'), function(e) { return e.id; }).join(' ')")
  expect_equal(jsv(count_models), 1, info = ids)
})
cat("\nSmoke test finished.\n")
