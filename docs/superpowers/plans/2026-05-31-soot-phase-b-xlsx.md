# SOOT Phase B Implementation Plan — XLSX Import + Response Rate

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Teach the existing ingestion layer to read the new per-respondent XLSX export by normalizing it into the same canonical long-format table the CSV reader produces, and add a response-rate-by-course chart that the XLSX metadata makes possible.

**Architecture:** One new reader function (`read_soot_xlsx`) plugs into the existing `read_soot_file` dispatch. Nothing downstream changes: all Phase A charts and helpers consume the canonical schema unchanged. The XLSX carries `Enrollments`/`Respondents`/`ResponseRate`, which were NA for CSV, so a new response-rate chart renders for XLSX-sourced courses and shows a placeholder otherwise.

**Tech Stack:** R, Shiny, tidyverse (dplyr/tidyr), readxl (read XLSX), writexl (build the test fixture), ggplot2, testthat.

**Builds on:** Phase A (merged to main). `R/helpers.R` already has `read_soot_csv`, `read_soot_file` (currently rejects XLSX), `parse_course_term`, `order_terms`, `compute_top_two_box`, `compute_rating_distribution`, the plot helpers, and constants `ANS_LEVELS`, `INSTRUCTOR_QUESTIONS`.

---

## Confirmed XLSX schema (from the real sample files)

Two sheets:
- `RawData`: one row per respondent. 21 metadata columns then `Question 1`..`Question 23`. `readxl` reads the `Question N` columns as **character** ("5","4",...) with blank cells as `""` or `NA` (no response). Metadata columns used: `Enrollments`, `Respondents`, `ResponseRate`, `InstructorName` (constant down the file).
- `QuestionMapper`: column 1 = "Question N", column 2 = full question text. Instructor items 1-9 end with the exact CSV question string after the final `" - "` separator. Non-instructor items (10-23) have other text and other answer scales.

Answer code mapping (define once in `R/helpers.R`):

```r
CODE_TO_ANS <- c(`0` = "Not Applicable", `1` = "Very Low or Never",
                 `2` = "Low", `3` = "Average", `4` = "High",
                 `5` = "Very High or Always")
```

`ANS_PCT` for XLSX is computed as `100 * count / (respondents who answered that question, including code 0)`, matching the CSV's semantics. Downstream charts recompute from `ANS_COUNT` anyway, so this is for schema parity and the data download.

---

## File structure

- Modify `R/helpers.R` — add `strip_question_prefix()`, `CODE_TO_ANS`, `read_soot_xlsx()`; change the `read_soot_file` dispatch so xlsx/xls routes to `read_soot_xlsx`; add `plot_response_rate()`.
- Create `tests/testthat/fixtures/make_xlsx_fixture.R` — generator for the synthetic fixture (committed for provenance).
- Create `tests/testthat/fixtures/TEST101-01_FALL25.xlsx` — synthetic, PII-free fixture (committed).
- Modify `tests/testthat/test-helpers.R` — replace the "rejects XLSX" test; add XLSX reader, mixed-batch, and response-rate tests.
- Modify `app.R` — add the response-rate UI section + server outputs; update the upload help text to mention XLSX.
- Modify `README.md` — note XLSX support and the response-rate chart.

---

## Task B1: Synthetic XLSX fixture

**Files:**
- Create: `tests/testthat/fixtures/make_xlsx_fixture.R`
- Create: `tests/testthat/fixtures/TEST101-01_FALL25.xlsx`

- [ ] **Step 1: Write the fixture generator**

Create `tests/testthat/fixtures/make_xlsx_fixture.R`:

```r
# Generates a small synthetic, PII-free SOOT XLSX fixture mirroring the real
# two-sheet structure (RawData + QuestionMapper). Run from this directory:
#   Rscript make_xlsx_fixture.R
library(writexl)

# Two instructor questions plus one non-instructor question that must be ignored.
mapper <- data.frame(
  Column = c("Question 1", "Question 2", "Question 3"),
  Question = c(
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - The instructor is well prepared for class.",
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - Overall, the instructor is an effective teacher.",
    "Year in School."
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)

raw <- data.frame(
  CourseCode = rep("99999.202590", 4),
  CourseTitle = rep("Fall 2025 Test Course (TEST-101-01)", 4),
  InstructorName = rep("Test Instructor", 4),
  Enrollments = rep(20, 4),
  Respondents = rep(4, 4),
  ResponseRate = rep(20.0, 4),
  "Question 1" = c("5", "4", "4", ""),     # VeryHigh, High, High, no-response
  "Question 2" = c("5", "3", "0", "2"),    # VeryHigh, Average, NotApplicable, Low
  "Question 3" = c("2", "3", "4", "1"),    # Year in School - must be ignored
  check.names = FALSE, stringsAsFactors = FALSE
)

write_xlsx(list(RawData = raw, QuestionMapper = mapper),
           path = "TEST101-01_FALL25.xlsx")
cat("wrote TEST101-01_FALL25.xlsx\n")
```

- [ ] **Step 2: Generate the fixture**

Run:
```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs/tests/testthat/fixtures && Rscript make_xlsx_fixture.R
```
Expected: prints `wrote TEST101-01_FALL25.xlsx`; the file exists.

- [ ] **Step 3: Verify the fixture reads back with the expected structure**

Run:
```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e '
  suppressMessages(library(readxl))
  f <- "tests/testthat/fixtures/TEST101-01_FALL25.xlsx"
  cat("sheets:", paste(excel_sheets(f), collapse=","), "\n")
  rd <- read_excel(f, "RawData"); cat("RawData dim:", nrow(rd), "x", ncol(rd), "\n")
  cat("Q1:", paste(rd[["Question 1"]], collapse=","), "\n")
'
```
Expected: sheets `RawData,QuestionMapper`; RawData 4 x 9; Q1 values `5,4,4,` (last blank).

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/fixtures/make_xlsx_fixture.R tests/testthat/fixtures/TEST101-01_FALL25.xlsx
git commit -m "Add synthetic PII-free XLSX test fixture and generator"
```

---

## Task B2: read_soot_xlsx() + dispatch

**Files:**
- Modify: `R/helpers.R`
- Modify: `tests/testthat/test-helpers.R`

- [ ] **Step 1: Replace the Phase A "rejects XLSX" test and add reader tests**

In `tests/testthat/test-helpers.R`, DELETE this existing test block:

```r
test_that("read_soot_file rejects XLSX in Phase A", {
  expect_error(read_soot_file("whatever.xlsx", "ANTH243-01_FALL25.xlsx"),
               "not yet supported")
})
```

and replace it with:

```r
test_that("read_soot_file reads an XLSX into the canonical schema, instructor questions only", {
  res <- read_soot_file(fixture("TEST101-01_FALL25.xlsx"), "TEST101-01_FALL25.xlsx")
  expect_true(all(c("course","term","QUES_TEXT","ANS_TEXT","ANS_COUNT","ANS_PCT",
                    "enrollment","respondents","response_rate","instructor")
                  %in% names(res)))
  expect_equal(unique(res$course), "TEST101")
  expect_equal(unique(res$term), "FALL25")
  expect_true(is.ordered(res$ANS_TEXT))
  # Only the two instructor questions survive; "Year in School." is excluded
  expect_setequal(unique(res$QUES_TEXT),
                  c("The instructor is well prepared for class.",
                    "Overall, the instructor is an effective teacher."))
  # Q1 counts: High = 2, Very High or Always = 1, blank dropped
  q1 <- res[res$QUES_TEXT == "The instructor is well prepared for class.", ]
  expect_equal(q1$ANS_COUNT[q1$ANS_TEXT == "High"], 2)
  expect_equal(q1$ANS_COUNT[q1$ANS_TEXT == "Very High or Always"], 1)
  # Metadata carried through from RawData
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
  expect_equal(q1$top_two_box, 100)         # (2+1)/3
  expect_equal(round(q2$top_two_box, 2), 33.33)  # 1 / 3 non-NA (0 excluded)
})

test_that("strip_question_prefix keeps text after the final ' - '", {
  expect_equal(strip_question_prefix(
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - The instructor is well prepared for class."),
    "The instructor is well prepared for class.")
  expect_equal(strip_question_prefix("Year in School."), "Year in School.")
})
```

- [ ] **Step 2: Run tests to verify failure**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — `strip_question_prefix`/`read_soot_xlsx` not found, and the XLSX read returns an error (still rejected).

- [ ] **Step 3: Implement in `R/helpers.R`**

Add `library(readxl)` near the other `library()` calls at the top. Add the `CODE_TO_ANS` constant next to `ANS_LEVELS`. Then append:

```r
# Take the question text after the final " - " separator (the instructor-item
# pattern); return the trimmed string unchanged if there is no such separator.
strip_question_prefix <- function(x) {
  trimws(sub("^.* - ", "", x))
}

# Read one per-respondent XLSX export into the canonical long format.
# Only the 9 instructor questions are normalized (other questions use different
# answer scales). Blank/no-response cells are dropped; code 0 is "Not Applicable".
read_soot_xlsx <- function(path, name) {
  raw <- tryCatch(readxl::read_excel(path, sheet = "RawData"),
                  error = function(e) NULL)
  mapper <- tryCatch(readxl::read_excel(path, sheet = "QuestionMapper"),
                     error = function(e) NULL)
  if (is.null(raw) || is.null(mapper) || nrow(raw) == 0) {
    warning(sprintf("Skipping empty or unreadable XLSX: %s", name))
    return(NULL)
  }
  qmap <- tibble::tibble(col = mapper[[1]],
                         QUES_TEXT = strip_question_prefix(mapper[[2]])) |>
    filter(QUES_TEXT %in% INSTRUCTOR_QUESTIONS, col %in% names(raw))
  if (nrow(qmap) == 0) {
    warning(sprintf("No instructor questions found in XLSX: %s", name))
    return(NULL)
  }
  ct <- parse_course_term(name)
  meta_first <- function(col) if (col %in% names(raw)) raw[[col]][1] else NA

  long <- raw |>
    mutate(.rid = row_number()) |>
    select(.rid, all_of(qmap$col)) |>
    pivot_longer(-.rid, names_to = "col", values_to = "code") |>
    mutate(code = trimws(as.character(code))) |>
    filter(!is.na(code), code %in% names(CODE_TO_ANS)) |>
    left_join(qmap, by = "col") |>
    mutate(ANS_TEXT = unname(CODE_TO_ANS[code]))

  agg <- long |>
    group_by(QUES_TEXT, ANS_TEXT) |>
    summarise(ANS_COUNT = n(), .groups = "drop") |>
    group_by(QUES_TEXT) |>
    mutate(ANS_PCT = 100 * ANS_COUNT / sum(ANS_COUNT)) |>
    ungroup()

  agg |>
    mutate(course = ct$course,
           term = ct$term,
           ANS_TEXT = factor(ANS_TEXT, levels = ANS_LEVELS, ordered = TRUE),
           enrollment = as.numeric(meta_first("Enrollments")),
           respondents = as.numeric(meta_first("Respondents")),
           response_rate = as.numeric(meta_first("ResponseRate")),
           instructor = as.character(meta_first("InstructorName"))) |>
    select(course, term, QUES_TEXT, ANS_TEXT, ANS_COUNT, ANS_PCT,
           enrollment, respondents, response_rate, instructor)
}
```

Then change the dispatch in `read_soot_file`: replace the xlsx branch

```r
  } else if (ext %in% c("xlsx", "xls")) {
    stop("XLSX import not yet supported. Please upload the old-format CSV files for now.")
  } else {
```

with

```r
  } else if (ext %in% c("xlsx", "xls")) {
    read_soot_xlsx(path, name)
  } else {
```

- [ ] **Step 4: Run tests to verify pass**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS, 0 failures (the new XLSX, top-two-box, and strip tests pass; all prior tests still pass).

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add XLSX reader normalizing per-respondent data to canonical schema"
```

---

## Task B3: Real-file smoke test + mixed CSV/XLSX batch

**Files:**
- Modify: `tests/testthat/test-helpers.R`

- [ ] **Step 1: Add a mixed-batch test using the committed fixtures**

The fixtures dir has CSVs (ANTH243-01_FALL24.csv etc.) and the new XLSX. Append:

```r
test_that("a mixed CSV + XLSX batch ingests through one canonical pipeline", {
  files <- c("ANTH243-01_FALL24.csv", "TEST101-01_FALL25.xlsx")
  full <- do.call(dplyr::bind_rows,
                  lapply(files, function(f) read_soot_file(fixture(f), f)))
  expect_true(all(c("ANTH243", "TEST101") %in% full$course))
  irq <- dplyr::filter(full, QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
  # CSV rows have NA metadata; XLSX rows carry a response_rate
  expect_true(any(is.na(irq$response_rate)))
  expect_true(any(!is.na(irq$response_rate)))
})
```

- [ ] **Step 2: Add a real-file smoke test that skips if the gitignored sample is absent**

Append:

```r
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
```

- [ ] **Step 3: Run tests**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS. The real-file test runs (sample present) and reads an actual XLSX; if the sample were absent it would skip, not fail.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test-helpers.R
git commit -m "Add mixed-batch and real-XLSX smoke tests"
```

---

## Task B4: Response-rate-by-course chart

**Files:**
- Modify: `R/helpers.R` (plot function), `app.R` (UI + server)

- [ ] **Step 1: Add a test for the plot's data behavior**

Append to `tests/testthat/test-helpers.R`:

```r
test_that("response_rate_by_course summarizes available rates and ignores NA-only", {
  d <- tibble::tibble(
    course = c("A","A","B","C"), term = c("FALL24","FALL24","FALL24","FALL24"),
    response_rate = c(80, 80, 40, NA_real_)
  )
  rr <- response_rate_by_course(d)
  expect_equal(sort(rr$course), c("A","B"))           # C has only NA -> excluded
  expect_equal(rr$response_rate[rr$course == "A"], 80) # de-duplicated per course/term
})
```

- [ ] **Step 2: Run to verify failure**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — `response_rate_by_course` not found.

- [ ] **Step 3: Implement the data helper and plot in `R/helpers.R`**

```r
# Mean response rate per course/term, dropping rows without a rate (CSV-sourced).
response_rate_by_course <- function(data) {
  data |>
    filter(!is.na(response_rate)) |>
    distinct(course, term, response_rate) |>
    group_by(course, term) |>
    summarise(response_rate = mean(response_rate), .groups = "drop")
}

# Bar of response rate per course (grouped by term). Renders a placeholder when
# no rate is available (e.g. only old-format CSV files were uploaded).
plot_response_rate <- function(data) {
  rr <- response_rate_by_course(data)
  if (nrow(rr) == 0) {
    return(ggplot() +
             annotate("text", x = 1, y = 1,
                      label = "Response rate not available\n(requires XLSX-format uploads)") +
             theme_void())
  }
  rr <- rr |> mutate(term = order_terms(term))
  ggplot(rr, aes(x = course, y = response_rate, fill = term)) +
    geom_col(position = position_dodge(preserve = "single")) +
    scale_fill_viridis_d() +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "", y = "Response rate (%)", fill = "Term",
         title = "Response Rate by Course") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 8))
}
```

- [ ] **Step 4: Add the UI section in `app.R`** (immediately before `h2("Course Inventory"),`)

```r
    h2("Response Rate by Course"),
    p("Percentage of enrolled students who responded, available for XLSX-format
      uploads (the older CSV export does not include enrollment)."),
    fluidRow(plotOutput("response_rate_plot")),
    fluidRow(
        downloadButton("downloadResponseRatePlot", "Download Response Rate Plot"),
        downloadButton("downloadResponseRateData", "Download Response Rate Data")
    ),
```

- [ ] **Step 5: Add the server outputs in `app.R`** (immediately before `#Create a course inventory`)

```r
        response_rate_plot <- plot_response_rate(instructor_related_ques)
        response_rate_data <- response_rate_by_course(instructor_related_ques)

        output$response_rate_plot <- renderPlot({ response_rate_plot })
        output$downloadResponseRatePlot <- downloadHandler(
            filename = "ResponseRateByCourse.png",
            content = function(file) { ggsave(file, response_rate_plot, width = 7, height = 5, dpi = 300) })
        output$downloadResponseRateData <- downloadHandler(
            filename = "response_rate_by_course.csv",
            content = function(file) { write.table(response_rate_data, file, sep = ",", row.names = FALSE) })
```

- [ ] **Step 6: Verify**

Run tests: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R` (expect PASS).
Parse: `cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e 'invisible(parse("app.R")); cat("parses\n")'`
Render both branches (with-data and placeholder):
```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e '
  suppressMessages({library(ggplot2); source("R/helpers.R")})
  x <- read_soot_file("tests/testthat/fixtures/TEST101-01_FALL25.xlsx","TEST101-01_FALL25.xlsx")
  ggsave(tempfile(fileext=".png"), plot_response_rate(x), width=7, height=5)
  csv <- read_soot_file("tests/testthat/fixtures/ANTH243-01_FALL24.csv","ANTH243-01_FALL24.csv")
  ggsave(tempfile(fileext=".png"), plot_response_rate(csv), width=7, height=5)  # placeholder branch
  cat("RESPONSE RATE PLOT (both branches) RENDERED OK\n")
'
```
Expected: "RESPONSE RATE PLOT (both branches) RENDERED OK".

- [ ] **Step 7: Commit**

```bash
git add R/helpers.R app.R
git commit -m "Add response-rate-by-course chart (XLSX-sourced)"
```

---

## Task B5: End-to-end verification, help text, README

**Files:**
- Modify: `app.R` (upload help text), `README.md`

- [ ] **Step 1: Update the upload help text in app.R**

In the UI, the `fileInput("csvs", label="Upload SOOT CSV files here", ...)` label and nearby instructional `p(...)` text say "CSV". Update the `fileInput` label to `"Upload SOOT CSV or XLSX files here"`. Do not change the input id (`csvs`). Leave other prose as-is unless it explicitly says only CSV is accepted, in which case add "or XLSX".

- [ ] **Step 2: Full headless end-to-end against the complete sample set (CSV + XLSX together)**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs
rm -rf /tmp/soot_b && mkdir /tmp/soot_b && unzip -q sample-data/sample-data.zip -d /tmp/soot_b
Rscript -e '
  suppressMessages({library(ggplot2); source("R/helpers.R")})
  dir <- "/tmp/soot_b/SOOTs"
  files <- list.files(dir, pattern="\\.(csv|xlsx)$", full.names=TRUE)
  cat("files:", length(files), "\n")
  full <- NULL; skipped <- 0
  for (f in files) {
    one <- withCallingHandlers(
      tryCatch(read_soot_file(f, basename(f)), error=function(e) NULL),
      warning=function(w){ skipped <<- skipped + 1; invokeRestart("muffleWarning") })
    if (!is.null(one)) full <- dplyr::bind_rows(full, one)
  }
  cat("skipped:", skipped, " rows:", nrow(full), "\n")
  irq <- dplyr::filter(full, QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
  cat("courses:", length(unique(irq$course)),
      " with response_rate:", sum(!is.na(unique(irq$response_rate))), "\n")
  od <- tempdir()
  ggsave(file.path(od,"rr.png"), plot_response_rate(irq), width=7, height=5)
  ggsave(file.path(od,"sum.png"), plot_summary_by_question(compute_top_two_box(irq,"QUES_TEXT","student"),"Student-Weighted","n"), width=7,height=5)
  ggsave(file.path(od,"hm.png"), plot_question_course_heatmap(irq), width=8,height=5)
  cat("E2E CSV+XLSX RENDER OK\n")
'
```
Expected: nonzero skipped (the 3 empty CSVs), a larger row count than Phase A (XLSX rows added), courses include XLSX-only ones, response_rate present for some, and "E2E CSV+XLSX RENDER OK". If anything errors, STOP and report BLOCKED.

- [ ] **Step 3: Confirm the app still boots**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs
Rscript -e 'shiny::runApp(".", port=7863, launch.browser=FALSE)' >/tmp/soot_b_app.log 2>&1 &
APP_PID=$!; sleep 8
curl -s -o /dev/null -w "HTTP %{http_code}\n" http://127.0.0.1:7863/ || echo "curl failed"
kill $APP_PID 2>/dev/null; cat /tmp/soot_b_app.log
```
Expected: HTTP 200, log shows "Listening on", no R error.

- [ ] **Step 4: Update README.md**

Add a sentence noting the app now accepts both the old aggregate CSV export and the new per-respondent XLSX export (both produce the same graphs), and that XLSX uploads additionally enable the response-rate-by-course chart. Keep existing content.

- [ ] **Step 5: Run the full unit suite once more**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: 0 failures.

- [ ] **Step 6: Commit**

```bash
git add app.R README.md
git commit -m "Accept XLSX uploads in UI; document XLSX support and response-rate chart"
```

---

## Self-review notes

- **Spec coverage (Phase B):** XLSX reader normalizing per-respondent data to the canonical schema (B2); same downstream code unchanged (no edits to Phase A charts); `readxl` dependency added (B2, top of helpers); response-rate chart enabled by XLSX metadata (B4). Edge cases: empty/unreadable XLSX -> warning+NULL (B2), no-instructor-questions XLSX -> warning+NULL (B2), blank responses dropped, code 0 kept as N/A, mixed CSV+XLSX batch (B3), response-rate placeholder when no XLSX (B4).
- **PII:** only the synthetic fixture is committed; real XLSX (instructor names, student IPs) stays in the gitignored zip and is used only via skip-if-present smoke test.
- **Type consistency:** `read_soot_xlsx` returns the identical 10-column canonical schema as `read_soot_csv` (ordered-factor `ANS_TEXT`, same column names/order). `response_rate_by_course` returns columns `course, term, response_rate`; `plot_response_rate` consumes them. `strip_question_prefix`, `CODE_TO_ANS`, `read_soot_xlsx`, `response_rate_by_course`, `plot_response_rate` are the only new names.
- **Deployment note (post-merge):** `readxl` is a new runtime dependency; it is already installed locally and will be snapshotted by `rsconnect::deployApp()` at deploy time. No manifest in the repo to update.
