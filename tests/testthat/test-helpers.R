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

test_that("a mixed CSV + XLSX batch ingests through one canonical pipeline", {
  files <- c("ANTH243-01_FALL24.csv", "TEST101-01_FALL25.xlsx")
  full <- do.call(dplyr::bind_rows,
                  lapply(files, function(f) read_soot_file(fixture(f), f)))
  expect_true(all(c("ANTH243", "TEST101") %in% full$course))
  irq <- dplyr::filter(full, QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
  expect_true(any(is.na(irq$response_rate)))
  expect_true(any(!is.na(irq$response_rate)))
})

test_that("the real sample XLSX reads without error (skipped if sample absent)", {
  zip <- file.path("..", "..", "sample-data", "sample-data.zip")
  skip_if_not(file.exists(zip), "sample-data.zip not present")
  tmp <- tempfile(); dir.create(tmp)
  utils::unzip(zip, exdir = tmp)
  xlsx <- list.files(file.path(tmp, "SOOTs"), pattern = "\\.xlsx$", full.names = TRUE)
  skip_if(length(xlsx) == 0, "no sample XLSX")
  res <- read_soot_file(xlsx[1], basename(xlsx[1]))
  expect_true(nrow(res) > 0)
  expect_true(all(res$QUES_TEXT %in% INSTRUCTOR_QUESTIONS))
  expect_true(is.numeric(res$response_rate))
})

test_that("response_rate_by_course summarizes available rates and ignores NA-only", {
  d <- tibble::tibble(
    course = c("A","A","B","C"), term = c("FALL24","FALL24","FALL24","FALL24"),
    response_rate = c(80, 80, 40, NA_real_)
  )
  rr <- response_rate_by_course(d)
  expect_equal(sort(rr$course), c("A","B"))
  expect_equal(rr$response_rate[rr$course == "A"], 80)
})

test_that("expand_uploads passes through loose files unchanged", {
  res <- expand_uploads(c("/tmp/a.csv", "/tmp/b.xlsx"), c("a.csv", "b.xlsx"))
  expect_equal(res$name, c("a.csv", "b.xlsx"))
  expect_equal(res$path, c("/tmp/a.csv", "/tmp/b.xlsx"))
})

test_that("expand_uploads extracts CSV/XLSX from a zip (incl nested dirs, ignoring mac cruft)", {
  d <- tempfile("zipsrc_"); dir.create(file.path(d, "sub"), recursive = TRUE)
  dir.create(file.path(d, "__MACOSX"))
  writeLines("x", file.path(d, "ANTH243-01_FALL24.csv"))
  writeLines("y", file.path(d, "sub", "ANTH482C-01_SPG25.xlsx"))
  writeLines("z", file.path(d, "__MACOSX", "._ANTH243-01_FALL24.csv"))
  zpath <- tempfile(fileext = ".zip")
  withr::with_dir(d, utils::zip(zpath, files = list.files(".", recursive = TRUE), flags = "-q"))
  res <- expand_uploads(zpath, "bundle.zip")
  expect_setequal(res$name, c("ANTH243-01_FALL24.csv", "ANTH482C-01_SPG25.xlsx"))
  expect_true(all(file.exists(res$path)))
})

test_that("expand_uploads handles a mixed loose-file + zip batch", {
  d <- tempfile("zipsrc2_"); dir.create(d)
  writeLines("x", file.path(d, "ANTH243-01_FALL19.csv"))
  zpath <- tempfile(fileext = ".zip")
  withr::with_dir(d, utils::zip(zpath, files = list.files("."), flags = "-q"))
  res <- expand_uploads(c("/tmp/loose.csv", zpath), c("loose.csv", "bundle.zip"))
  expect_setequal(res$name, c("loose.csv", "ANTH243-01_FALL19.csv"))
})

test_that("build_report_pdf writes a multi-page PDF: cover + one page per chart", {
  library(ggplot2)
  plots <- list(
    "Chart One" = ggplot(mtcars, aes(mpg, wt)) + geom_point(),
    "Chart Two" = ggplot(mtcars, aes(hp, qsec)) + geom_point()
  )
  meta <- list(courses = c("ANTH243","ANTH482C"), terms = c("FALL24","SPG25"),
               instructor = "Test Instructor", n_courses = 2, date = as.Date("2026-05-31"))
  f <- tempfile(fileext = ".pdf")
  build_report_pdf(f, plots, meta)
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 0)
  if (requireNamespace("pdftools", quietly = TRUE)) {
    expect_equal(pdftools::pdf_info(f)$pages, 3)
  }
})

test_that("build_report_pdf writes a one-page placeholder when there is no data", {
  f <- tempfile(fileext = ".pdf")
  build_report_pdf(f, list(), NULL)
  expect_true(file.exists(f))
  if (requireNamespace("pdftools", quietly = TRUE)) {
    expect_equal(pdftools::pdf_info(f)$pages, 1)
  }
})

test_that("upload_summary counts files, courses, terms, and respondents", {
  full <- tibble::tibble(
    course = c("A","A","B"), term = c("FALL24","FALL24","SPG25"),
    QUES_TEXT = "Q", ANS_TEXT = "High", ANS_COUNT = c(10, 5, 8),
    instructor = NA_character_
  )
  s <- upload_summary(full, n_uploaded = 4, n_skipped = 1)
  expect_equal(s$files_processed, 3)
  expect_equal(s$files_skipped, 1)
  expect_equal(s$n_courses, 2)
  expect_equal(s$n_terms, 2)
  expect_equal(s$respondents, 23)
})

test_that("distinct_instructors returns non-NA unique names", {
  full <- tibble::tibble(instructor = c("Smith","Smith", NA, "Jones"))
  expect_setequal(distinct_instructors(full), c("Smith","Jones"))
  expect_length(distinct_instructors(tibble::tibble(instructor = c(NA_character_, NA))), 0)
})

test_that("read_context_csv extracts curated questions with their labels", {
  res <- read_context_csv(fixture("ANTH243-01_FALL24.csv"), "ANTH243-01_FALL24.csv")
  expect_true(all(c("course","term","QUES_TEXT","ANS_TEXT","count") %in% names(res)))
  diff <- res[res$QUES_TEXT == "Difficulty (relative to other courses)", ]
  expect_setequal(diff$ANS_TEXT, c("Low","Medium"))
  expect_equal(sum(diff$count), 38)
  after <- res[res$QUES_TEXT == "My interest in subject after course", ]
  expect_equal(after$count[after$ANS_TEXT == "High"], 19)
  expect_false("Year in School" %in% res$QUES_TEXT)
})

test_that("read_context_xlsx decodes curated questions to labels and counts", {
  res <- read_context_xlsx(fixture("TEST101-01_FALL25.xlsx"), "TEST101-01_FALL25.xlsx")
  expect_true(all(c("course","term","QUES_TEXT","ANS_TEXT","count") %in% names(res)))
  before <- res[res$QUES_TEXT == "My interest in subject before course", ]
  expect_equal(before$count[before$ANS_TEXT == "Low"], 2)
  expect_equal(before$count[before$ANS_TEXT == "Medium"], 2)
  after <- res[res$QUES_TEXT == "My interest in subject after course", ]
  expect_equal(after$count[after$ANS_TEXT == "High"], 3)
  expect_false("Year in School." %in% res$QUES_TEXT)
})

test_that("read_context_file dispatches on extension", {
  csvres <- read_context_file(fixture("ANTH243-01_FALL24.csv"), "ANTH243-01_FALL24.csv")
  expect_true("Difficulty (relative to other courses)" %in% csvres$QUES_TEXT)
  xres <- read_context_file(fixture("TEST101-01_FALL25.xlsx"), "TEST101-01_FALL25.xlsx")
  expect_true("My interest in subject after course" %in% xres$QUES_TEXT)
})

test_that("read_context_xlsx matches the real sample mapper text with trailing periods (skipped if absent)", {
  zip <- file.path("..", "..", "sample-data", "sample-data.zip")
  skip_if_not(file.exists(zip), "sample-data.zip not present")
  tmp <- tempfile(); dir.create(tmp); utils::unzip(zip, exdir = tmp)
  xlsx <- list.files(file.path(tmp, "SOOTs"), pattern = "[.]xlsx$", full.names = TRUE)
  skip_if(length(xlsx) == 0, "no sample XLSX")
  res <- read_context_xlsx(xlsx[1], basename(xlsx[1]))
  expect_false(is.null(res))
  expect_gt(nrow(res), 0)
  # the real mapper text carries trailing periods; matching must still find these
  expect_true("My interest in subject after course" %in% res$QUES_TEXT)
  expect_true("Expected Grade" %in% res$QUES_TEXT)
})

test_that("context plot helpers render against fixture data and handle empty input", {
  library(ggplot2)
  ctx <- read_context_file(fixture("ANTH243-01_FALL24.csv"), "ANTH243-01_FALL24.csv")
  for (p in list(plot_interest_shift(ctx), plot_course_demands(ctx),
                 plot_expected_grade(ctx), plot_material_usefulness(ctx))) {
    f <- tempfile(fileext = ".png"); ggsave(f, p, width = 6, height = 4); expect_gt(file.info(f)$size, 0)
  }
  for (p in list(plot_interest_shift(NULL), plot_course_demands(NULL),
                 plot_expected_grade(NULL), plot_material_usefulness(NULL))) {
    f <- tempfile(fileext = ".png"); ggsave(f, p, width = 6, height = 4); expect_gt(file.info(f)$size, 0)
  }
})
