# Run from the repository folder:  Rscript -e 'testthat::test_file("tests/test-app-helpers.R")'
library(testthat)
source("helpers-app.R")
h <- load_app_helpers("../app.R")

test_that("a chain of equalities stands for all its pairs", {
  expect_equal(h$expand_clause_to_rows("p1=p2=p3"), c("p1=p2", "p1=p3", "p2=p3"))
  expect_equal(length(h$expand_clause_to_rows("p1=p2=p3=p4=p5")), 10L)
  expect_equal(h$expand_clause_to_rows("p1=p2"), "p1=p2")
})

test_that("numbers and expressions are terms of a chain, number = number is dropped", {
  expect_equal(h$expand_clause_to_rows("p1=p2=0.5"), c("p1=p2", "p1=0.5", "p2=0.5"))
  expect_equal(h$expand_clause_to_rows("0.5=0.3=p1"), c("0.5=p1", "0.3=p1"))
  expect_equal(h$expand_clause_to_rows("2*p1=p2=p3"), c("2*p1=p2", "2*p1=p3", "p2=p3"))
  expect_equal(h$expand_clause_to_rows("{p1,p2}=p3=p4"),
               c("p1=p2", "p1=p3", "p1=p4", "p2=p3", "p2=p4", "p3=p4"))
})

test_that("comparisons next to a chain keep their neighbours", {
  expect_equal(h$expand_clause_to_rows("p1=p2=p3>p4"), c("p1=p2", "p1=p3", "p2=p3", "p3>p4"))
  expect_equal(h$expand_clause_to_rows("p1>p2>p3"), c("p1>p2", "p2>p3"))
  expect_equal(h$clause_expected_row_count("p1=p2=p3>p4"), 4L)
})

test_that(">= and <= mean > and <", {
  expect_equal(h$expand_clause_to_rows("p1>=p2"), "p1>p2")
  expect_equal(h$expand_clause_to_rows("p1<=p2=p3=p4"), c("p1<p2", "p2=p3", "p2=p4", "p3=p4"))
})

test_that("p1 = p2 and p2 = p1 are one equality, p1 = p1 is dropped", {
  rows <- h$spec_eq_rows("{p1,p2,p3}={p1,p2,p3}; p1 = p1")
  expect_equal(length(rows), 3L)
  expect_equal(length(h$spec_eq_rows("p1=p2; p2=p1")), 1L)
  expect_equal(h$expand_equality_chains("p1=p2=p3; p4>p5"), "p1=p2; p1=p3; p2=p3; p4>p5")
})

test_that("tolerances follow their equality when the text changes", {
  old <- c("p1=p2", "p3=p4"); tol <- c(0, 0.05)
  expect_equal(h$align_tolerances(old, tol, c("p5=p6", "p1=p2", "p3=p4")), c(0, 0, 0.05))   # insert in front
  expect_equal(h$align_tolerances(old, tol, c("p1=p2", "p3=p7")), c(0, 0.05))                # edit in place
  expect_equal(h$align_tolerances(old, tol, c("p3=p4")), 0.05)                              # delete the first
})
