source(file.path("..", "..", "R", "helpers.R"))

test_that("constants are defined", {
  expect_length(ANS_LEVELS, 6)
  expect_length(INSTRUCTOR_QUESTIONS, 9)
  expect_true(all(TOP_BOX_LEVELS %in% ANS_LEVELS))
})

test_that("parse_course_term splits course and term from filename", {
  expect_equal(parse_course_term("ANTH243-01_FALL24.csv"),
               list(course = "ANTH243", term = "FALL24"))
  expect_equal(parse_course_term("ANTH482C-01_SPG25.csv"),
               list(course = "ANTH482C", term = "SPG25"))
  expect_equal(parse_course_term("ANTH243-90_SPG21.csv"),
               list(course = "ANTH243", term = "SPG21"))
})

test_that("order_terms returns a chronological ordered factor", {
  res <- order_terms(c("FALL24", "SPG25", "FALL19", "SPG16"))
  expect_s3_class(res, "factor")
  expect_true(is.ordered(res))
  expect_equal(levels(res), c("SPG16", "FALL19", "FALL24", "SPG25"))
})

test_that("order_terms keeps spring before fall within a year", {
  res <- order_terms(c("FALL21", "SPG21"))
  expect_equal(levels(res), c("SPG21", "FALL21"))
})

test_that("order_terms handles a single term", {
  res <- order_terms("FALL20")
  expect_equal(levels(res), "FALL20")
})
