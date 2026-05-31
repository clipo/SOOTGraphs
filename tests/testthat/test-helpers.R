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
