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

test_that("read_soot_file reads an XLSX into the canonical schema, instructor questions only", {
  res <- read_soot_file(fixture("TEST101-01_FALL25.xlsx"), "TEST101-01_FALL25.xlsx")
  expect_true(all(c("course","term","QUES_TEXT","ANS_TEXT","ANS_COUNT","ANS_PCT",
                    "enrollment","respondents","response_rate","instructor")
                  %in% names(res)))
  expect_equal(unique(res$course), "TEST101")
  expect_equal(unique(res$term), "FALL25")
  expect_true(is.ordered(res$ANS_TEXT))
  expect_setequal(unique(res$QUES_TEXT),
                  c("The instructor is well prepared for class.",
                    "Overall, the instructor is an effective teacher."))
  q1 <- res[res$QUES_TEXT == "The instructor is well prepared for class.", ]
  expect_equal(q1$ANS_COUNT[q1$ANS_TEXT == "High"], 2)
  expect_equal(q1$ANS_COUNT[q1$ANS_TEXT == "Very High or Always"], 1)
  expect_equal(unique(res$response_rate), 20)
  expect_equal(unique(res$respondents), 4)
  expect_equal(unique(res$enrollment), 20)
  expect_equal(unique(res$instructor), "Test Instructor")
})

test_that("top-two-box from the XLSX fixture is correct", {
  res <- read_soot_file(fixture("TEST101-01_FALL25.xlsx"), "TEST101-01_FALL25.xlsx")
  ttb <- compute_top_two_box(res, "QUES_TEXT", "student")
  q1 <- ttb[ttb$QUES_TEXT == "The instructor is well prepared for class.", ]
  q2 <- ttb[ttb$QUES_TEXT == "Overall, the instructor is an effective teacher.", ]
  expect_equal(q1$top_two_box, 100)
  expect_equal(round(q2$top_two_box, 2), 33.33)
})

test_that("strip_question_prefix keeps text after the final ' - '", {
  expect_equal(strip_question_prefix(
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - The instructor is well prepared for class."),
    "The instructor is well prepared for class.")
  expect_equal(strip_question_prefix("Year in School."), "Year in School.")
})

make_canonical <- function() {
  tibble::tibble(
    course = c(rep("A", 3), rep("B", 3)),
    term   = "FALL24",
    QUES_TEXT = "Qx",
    ANS_TEXT = factor(c("Average","High","Very High or Always",
                        "Low","High","Very High or Always"),
                      levels = ANS_LEVELS, ordered = TRUE),
    ANS_COUNT = c(2, 3, 5,   18, 1, 1)
  )
}

test_that("compute_top_two_box student-weighted pools counts", {
  res <- compute_top_two_box(make_canonical(), group_vars = "QUES_TEXT",
                             weighting = "student")
  expect_equal(round(res$top_two_box, 2), 33.33)  # (8) / (10+20) * 100
  expect_equal(res$n, 30)                          # non-NA respondents pooled
})

test_that("compute_top_two_box course-weighted averages course scores equally", {
  res <- compute_top_two_box(make_canonical(), group_vars = "QUES_TEXT",
                             weighting = "course")
  expect_equal(round(res$top_two_box, 2), 45.00)   # mean(80, 10)
  expect_equal(res$n_courses, 2)
})

test_that("compute_top_two_box returns NA for an all-NA question", {
  d <- tibble::tibble(course = "A", term = "FALL24", QUES_TEXT = "Qx",
                      ANS_TEXT = factor("Not Applicable", levels = ANS_LEVELS,
                                        ordered = TRUE),
                      ANS_COUNT = 5)
  res <- compute_top_two_box(d, group_vars = "QUES_TEXT", weighting = "student")
  expect_true(is.na(res$top_two_box))
})

test_that("compute_rating_distribution excludes N/A and reports its share", {
  d <- tibble::tibble(
    course = "A", term = "FALL24", QUES_TEXT = "Qx",
    ANS_TEXT = factor(c("Not Applicable","Average","High","Very High or Always"),
                      levels = ANS_LEVELS, ordered = TRUE),
    ANS_COUNT = c(1, 2, 3, 5)
  )
  res <- compute_rating_distribution(d, group_vars = "QUES_TEXT")
  # Among the 10 non-NA ratings: Average 20%, High 30%, Very High 50%
  high <- res$dist[res$dist$ANS_TEXT == "High", ]
  expect_equal(high$pct, 30)
  expect_false("Not Applicable" %in% res$dist$ANS_TEXT)
  # N/A share is 1 of 11 total responses
  expect_equal(round(res$na_share$na_pct, 2), 9.09)
})
