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

fixture <- function(name) file.path("fixtures", name)

test_that("read_soot_file reads a CSV into the canonical schema", {
  res <- read_soot_file(fixture("ANTH243-01_FALL24.csv"), "ANTH243-01_FALL24.csv")
  expect_true(all(c("course","term","QUES_TEXT","ANS_TEXT","ANS_COUNT","ANS_PCT",
                    "enrollment","respondents","response_rate","instructor")
                  %in% names(res)))
  expect_equal(unique(res$course), "ANTH243")
  expect_equal(unique(res$term), "FALL24")
  expect_true(is.ordered(res$ANS_TEXT))
  expect_true(all(is.na(res$enrollment)))
  row <- res[res$QUES_TEXT == "The instructor is well prepared for class." &
             res$ANS_TEXT == "Very High or Always", ]
  expect_equal(row$ANS_COUNT, 18)
})

test_that("read_soot_file returns NULL for an empty file", {
  empty <- tempfile(fileext = ".csv")
  file.create(empty)
  expect_warning(res <- read_soot_file(empty, "EMPTY-01_FALL20.csv"))
  expect_null(res)
})

test_that("read_soot_file rejects XLSX in Phase A", {
  expect_error(read_soot_file("whatever.xlsx", "ANTH243-01_FALL25.xlsx"),
               "not yet supported")
})
