# SOOT Context Questions + Polish — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a post-upload summary, a multi-instructor warning, repo cleanup + CI (Phase 1), then surface the curated course-context questions as a new tab and in the PDF report via a parallel context pipeline (Phase 2).

**Architecture:** Phase 1 adds two tiny pure helpers plus server/UI wiring and housekeeping. Phase 2 adds a self-contained context pipeline (`CONTEXT_QUESTIONS`, `read_context_*`, four `plot_*` helpers) that reads the same uploaded files a second time and feeds a new "Course Context" tab; the instructor pipeline and all existing charts are untouched.

**Tech Stack:** R, Shiny, bslib, ggplot2, readxl, writexl (fixture), testthat, pdftools (tests).

**Builds on:** the deployed app on `main`. Server has `work <- expand_uploads(...)`, builds `full` (instructor canonical: course, term, QUES_TEXT, ANS_TEXT ordered factor, ANS_COUNT, ANS_PCT, enrollment, respondents, response_rate, instructor) and `instructor_related_ques`, then ten plot objects and `output$downloadReport` (real handler at app.R:384, before `#Create a course inventory` at app.R:410). The UI is `page_fluid` with `layout_sidebar` + `navset_card_tab` (four nav_panels).

---

# PHASE 1 — Polish

## Task 1: upload_summary + distinct_instructors helpers

**Files:** Modify `R/helpers.R`, `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write failing tests** — append to `tests/testthat/test-helpers.R`:

```r
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
  # respondents: A/FALL24 max question total = 15; B/SPG25 = 8; sum = 23
  expect_equal(s$respondents, 23)
})

test_that("distinct_instructors returns non-NA unique names", {
  full <- tibble::tibble(instructor = c("Smith","Smith", NA, "Jones"))
  expect_setequal(distinct_instructors(full), c("Smith","Jones"))
  expect_length(distinct_instructors(tibble::tibble(instructor = c(NA_character_, NA))), 0)
})
```

- [ ] **Step 2: Run to verify failure** — `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R` — FAIL (functions not found).

- [ ] **Step 3: Implement** — append to `R/helpers.R`:

```r
# Summary statistics for the post-upload confirmation panel.
upload_summary <- function(full, n_uploaded, n_skipped) {
  resp <- full |>
    group_by(course, term, QUES_TEXT) |>
    summarise(n = sum(ANS_COUNT), .groups = "drop") |>
    group_by(course, term) |>
    summarise(respondents = max(n), .groups = "drop")
  list(
    files_processed = n_uploaded - n_skipped,
    files_skipped   = n_skipped,
    n_courses       = length(unique(full$course)),
    n_terms         = length(unique(full$term)),
    respondents     = sum(resp$respondents)
  )
}

# Distinct non-missing instructor names present in the data (XLSX-sourced).
distinct_instructors <- function(full) {
  ins <- unique(full$instructor)
  ins[!is.na(ins)]
}
```

- [ ] **Step 4: Run to verify pass** — expect PASS.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add upload_summary and distinct_instructors helpers"
```

---

## Task 2: Wire summary panel, skipped counter, and multi-instructor warning

**Files:** Modify `app.R` (UI: add `uiOutput`; server: skipped counter, summary render, warning)

- [ ] **Step 1: Add the summary output to the UI**

In `app.R`, in the `layout_sidebar(...)` main area, insert `uiOutput("upload_summary")` as the FIRST main-area child, immediately before `navset_card_tab(`:

```r
        uiOutput("upload_summary"),
        navset_card_tab(
```

- [ ] **Step 2: Track skipped files in the ingestion loop**

Replace the ingestion loop body so it counts skips. The current loop is:

```r
        work <- expand_uploads(input$csvs$datapath, input$csvs$name)
        full <- NULL
        for (i in seq_len(nrow(work))) {
            one <- tryCatch(
                read_soot_file(work$path[i], work$name[i]),
                error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
            )
            if (!is.null(one)) full <- bind_rows(full, one)
        }
```

Replace with (adds `n_skipped`):

```r
        work <- expand_uploads(input$csvs$datapath, input$csvs$name)
        full <- NULL
        n_skipped <- 0
        for (i in seq_len(nrow(work))) {
            one <- tryCatch(
                read_soot_file(work$path[i], work$name[i]),
                error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
            )
            if (!is.null(one)) full <- bind_rows(full, one) else n_skipped <- n_skipped + 1
        }
```

- [ ] **Step 3: Render the summary and warn on multiple instructors**

Immediately after the `if (is.null(full) || nrow(full) == 0) { ... return() }` guard, add:

```r
        # Post-upload summary panel
        summ <- upload_summary(full, n_uploaded = nrow(work), n_skipped = n_skipped)
        output$upload_summary <- renderUI({
            bslib::layout_columns(
                fill = FALSE,
                bslib::value_box("Files processed", summ$files_processed, theme = "success"),
                bslib::value_box("Courses", summ$n_courses, theme = "primary"),
                bslib::value_box("Terms", summ$n_terms, theme = "primary"),
                bslib::value_box("Respondents", summ$respondents, theme = "secondary"),
                bslib::value_box("Files skipped", summ$files_skipped,
                                 theme = if (summ$files_skipped > 0) "warning" else "light")
            )
        })

        # Warn if the uploaded XLSX files combine multiple instructors
        instructors <- distinct_instructors(full)
        if (length(instructors) > 1) {
            showNotification(
                paste0("These files combine ", length(instructors), " instructors (",
                       paste(instructors, collapse = ", "),
                       "). Results are aggregated across all of them."),
                type = "warning", duration = NULL)
        }
```

- [ ] **Step 4: Verify parse and boot**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e 'invisible(parse("app.R")); cat("parses\n")'
Rscript -e 'shiny::runApp(".", port=7866, launch.browser=FALSE)' >/tmp/p1.log 2>&1 &
P=$!; sleep 8; curl -s -o /dev/null -w "HTTP %{http_code}\n" http://127.0.0.1:7866/ || echo fail
kill $P 2>/dev/null; grep -i "error\|listening" /tmp/p1.log | head
```
Expected: `parses`; HTTP 200; "Listening on"; no R error.

- [ ] **Step 5: Commit**

```bash
git add app.R
git commit -m "Add post-upload summary panel and multi-instructor warning"
```

---

## Task 3: Cleanup and CI

**Files:** Delete `app2.R`; Modify `app.R` (libs + favicon); Create `.github/workflows/tests.yml`

- [ ] **Step 1: Remove the stale predecessor and unused libraries**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && git rm app2.R
```
In `app.R`, delete the lines `library(data.table)` and `library(purrr)` (both confirmed unused).

- [ ] **Step 2: Add a favicon link (reuse the logo)**

In `app.R`, inside the existing `tags$head(...)` (which holds the `<style>`), add a favicon link as the first child of `tags$head`:

```r
    tags$head(
      tags$link(rel = "icon", href = "BU-logo.png"),
      tags$style(HTML("
```
(keep the existing CSS string and closing `))` intact; you are only inserting the `tags$link(...)` line and wrapping the existing `tags$style(...)` as a second argument to `tags$head`).

- [ ] **Step 3: Create the CI workflow** — create `.github/workflows/tests.yml`:

```yaml
name: tests
on:
  push:
  pull_request:
jobs:
  testthat:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'
      - name: Install packages
        run: |
          install.packages(c("testthat","dplyr","tidyr","stringr","tibble",
                             "ggplot2","viridisLite","readxl","writexl","png",
                             "pdftools","withr"))
        shell: Rscript {0}
      - name: Run tests
        run: cd tests && Rscript testthat.R
```

- [ ] **Step 4: Verify the app still parses and boots, and tests pass**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e 'invisible(parse("app.R")); cat("parses\n")'
cd tests && Rscript testthat.R 2>&1 | grep -E "Fail|DONE" | tail -2
```
Expected: `parses`; tests show no failures. Also confirm `git status` shows `app2.R` deleted and `.github/workflows/tests.yml` added.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "Remove stale app2.R and unused libs; add favicon and CI workflow"
```

---

# PHASE 2 — Course context questions

## Task 4: CONTEXT_QUESTIONS + context_empty_plot + read_context_csv

**Files:** Modify `R/helpers.R`, `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write failing tests** — append to `tests/testthat/test-helpers.R`:

```r
test_that("read_context_csv extracts curated questions with their labels", {
  res <- read_context_csv(fixture("ANTH243-01_FALL24.csv"), "ANTH243-01_FALL24.csv")
  expect_true(all(c("course","term","QUES_TEXT","ANS_TEXT","count") %in% names(res)))
  diff <- res[res$QUES_TEXT == "Difficulty (relative to other courses)", ]
  expect_setequal(diff$ANS_TEXT, c("Low","Medium"))     # this course used only Low/Medium
  expect_equal(sum(diff$count), 38)
  after <- res[res$QUES_TEXT == "My interest in subject after course", ]
  expect_equal(after$count[after$ANS_TEXT == "High"], 19)
  # Year in School is NOT curated, so it must be absent
  expect_false("Year in School" %in% res$QUES_TEXT)
})
```

- [ ] **Step 2: Run to verify failure** — FAIL (`read_context_csv` not found).

- [ ] **Step 3: Implement** — append to `R/helpers.R`:

```r
# Curated course-context questions: canonical CSV text, chart label order, group,
# and the XLSX numeric-code-to-label map (validated against the CSV label
# vocabulary; the Expected Grade letter map is an explicit inference).
CONTEXT_QUESTIONS <- list(
  list(text = "My interest in subject before course", group = "interest",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "My interest in subject after course", group = "interest",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "Difficulty (relative to other courses)", group = "demands",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "Workload (relative to other courses)", group = "demands",
       labels = c("Low","Medium","High"),
       codes = c(`1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of texts", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of homework assignments", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of lab assignments", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of examinations", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Usefulness of class discussions", group = "usefulness",
       labels = c("Low","Medium","High"),
       codes = c(`0`="Not Applicable", `1`="Low", `2`="Medium", `3`="High")),
  list(text = "Expected Grade", group = "grade",
       labels = c("A","B","C","D","F","P","NP","Don't Know"),
       codes = c(`1`="A", `2`="B", `3`="C", `4`="D", `5`="F", `6`="P", `7`="NP", `8`="Don't Know"))
)

CONTEXT_TEXTS <- vapply(CONTEXT_QUESTIONS, `[[`, character(1), "text")

# A labeled empty-state placeholder for a context chart with no data.
context_empty_plot <- function(title) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 1, y = 1,
                      label = paste0(title, "\n(no data in the uploaded files)")) +
    ggplot2::theme_void()
}

# Read the curated context questions from a CSV (labels already present).
read_context_csv <- function(path, name) {
  raw <- tryCatch(read.csv(path, header = TRUE, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  ct <- parse_course_term(name)
  raw |>
    filter(QUES_TEXT %in% CONTEXT_TEXTS, ANS_COUNT > 0) |>
    transmute(course = ct$course, term = ct$term, QUES_TEXT,
              ANS_TEXT = as.character(ANS_TEXT), count = as.integer(ANS_COUNT)) |>
    tibble::as_tibble()
}
```

- [ ] **Step 4: Run to verify pass** — expect PASS.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add CONTEXT_QUESTIONS, context_empty_plot, and read_context_csv"
```

---

## Task 5: Extend fixture + read_context_xlsx + read_context_file

**Files:** Modify `tests/testthat/fixtures/make_xlsx_fixture.R`, regenerate the xlsx, Modify `R/helpers.R`, `tests/testthat/test-helpers.R`

- [ ] **Step 1: Extend the synthetic fixture generator**

In `tests/testthat/fixtures/make_xlsx_fixture.R`, add two context questions. Change the `mapper` data.frame to include rows for Question 4 and Question 5, and add the two columns to `raw`:

```r
mapper <- data.frame(
  Column = c("Question 1", "Question 2", "Question 3", "Question 4", "Question 5"),
  Question = c(
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - The instructor is well prepared for class.",
    "[InstructorName]-Instructor Effectiveness and Teaching Practices - Overall, the instructor is an effective teacher.",
    "Year in School.",
    "Course Interest and Demands - My interest in subject before course.",
    "Course Interest and Demands - My interest in subject after course."
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)
```
and add these two columns to the `raw` data.frame (after `"Question 3"`):

```r
  "Question 4" = c("1", "1", "2", "2"),   # interest before: Low,Low,Medium,Medium
  "Question 5" = c("3", "3", "2", "3"),   # interest after:  High,High,Medium,High
```

Then regenerate:
```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs/tests/testthat/fixtures && Rscript make_xlsx_fixture.R
```
Expected: `wrote TEST101-01_FALL25.xlsx`.

- [ ] **Step 2: Write failing tests** — append to `tests/testthat/test-helpers.R`:

```r
test_that("read_context_xlsx decodes curated questions to labels and counts", {
  res <- read_context_xlsx(fixture("TEST101-01_FALL25.xlsx"), "TEST101-01_FALL25.xlsx")
  expect_true(all(c("course","term","QUES_TEXT","ANS_TEXT","count") %in% names(res)))
  before <- res[res$QUES_TEXT == "My interest in subject before course", ]
  expect_equal(before$count[before$ANS_TEXT == "Low"], 2)
  expect_equal(before$count[before$ANS_TEXT == "Medium"], 2)
  after <- res[res$QUES_TEXT == "My interest in subject after course", ]
  expect_equal(after$count[after$ANS_TEXT == "High"], 3)
  expect_false("Year in School." %in% res$QUES_TEXT)  # not a curated question
})

test_that("read_context_file dispatches on extension", {
  csvres <- read_context_file(fixture("ANTH243-01_FALL24.csv"), "ANTH243-01_FALL24.csv")
  expect_true("Difficulty (relative to other courses)" %in% csvres$QUES_TEXT)
  xres <- read_context_file(fixture("TEST101-01_FALL25.xlsx"), "TEST101-01_FALL25.xlsx")
  expect_true("My interest in subject after course" %in% xres$QUES_TEXT)
})
```

- [ ] **Step 3: Run to verify failure** — FAIL (`read_context_xlsx` not found).

- [ ] **Step 4: Implement** — append to `R/helpers.R`:

```r
# Read the curated context questions from a per-respondent XLSX, decoding numeric
# codes to labels via CONTEXT_QUESTIONS. Blank/unmapped codes are dropped.
read_context_xlsx <- function(path, name) {
  raw <- tryCatch(readxl::read_excel(path, sheet = "RawData"), error = function(e) NULL)
  mapper <- tryCatch(readxl::read_excel(path, sheet = "QuestionMapper"), error = function(e) NULL)
  if (is.null(raw) || is.null(mapper) || nrow(raw) == 0) return(NULL)
  ct <- parse_course_term(name)
  qmap <- tibble::tibble(col = mapper[[1]],
                         QUES_TEXT = strip_question_prefix(mapper[[2]])) |>
    filter(QUES_TEXT %in% CONTEXT_TEXTS, col %in% names(raw))
  if (nrow(qmap) == 0) return(NULL)
  out <- list()
  for (k in seq_len(nrow(qmap))) {
    qtext <- qmap$QUES_TEXT[k]
    codes <- CONTEXT_QUESTIONS[[which(CONTEXT_TEXTS == qtext)[1]]]$codes
    vals <- trimws(as.character(raw[[qmap$col[k]]]))
    vals <- vals[!is.na(vals) & vals %in% names(codes)]
    if (length(vals) == 0) next
    tab <- as.data.frame(table(ANS_TEXT = unname(codes[vals])), stringsAsFactors = FALSE)
    out[[length(out) + 1]] <- tibble::tibble(
      course = ct$course, term = ct$term, QUES_TEXT = qtext,
      ANS_TEXT = tab$ANS_TEXT, count = as.integer(tab$Freq))
  }
  if (length(out) == 0) return(NULL)
  dplyr::bind_rows(out)
}

# Dispatch context reading on file extension.
read_context_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") read_context_csv(path, name)
  else if (ext %in% c("xlsx", "xls")) read_context_xlsx(path, name)
  else NULL
}
```

- [ ] **Step 5: Run to verify pass** — expect PASS (including the still-passing instructor tests, which ignore Q4/Q5).

- [ ] **Step 6: Commit**

```bash
git add R/helpers.R tests/testthat/fixtures/make_xlsx_fixture.R tests/testthat/fixtures/TEST101-01_FALL25.xlsx tests/testthat/test-helpers.R
git commit -m "Add read_context_xlsx and read_context_file; extend fixture with context questions"
```

---

## Task 6: Four context plot helpers

**Files:** Modify `R/helpers.R`, `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write failing tests** — append to `tests/testthat/test-helpers.R`:

```r
test_that("context plot helpers render against fixture data and handle empty input", {
  library(ggplot2)
  ctx <- read_context_file(fixture("ANTH243-01_FALL24.csv"), "ANTH243-01_FALL24.csv")
  for (p in list(plot_interest_shift(ctx), plot_course_demands(ctx),
                 plot_expected_grade(ctx), plot_material_usefulness(ctx))) {
    f <- tempfile(fileext = ".png"); ggsave(f, p, width = 6, height = 4); expect_gt(file.info(f)$size, 0)
  }
  # empty-state branch: NULL context must not error
  for (p in list(plot_interest_shift(NULL), plot_course_demands(NULL),
                 plot_expected_grade(NULL), plot_material_usefulness(NULL))) {
    f <- tempfile(fileext = ".png"); ggsave(f, p, width = 6, height = 4); expect_gt(file.info(f)$size, 0)
  }
})
```

- [ ] **Step 2: Run to verify failure** — FAIL (helpers not found).

- [ ] **Step 3: Implement** — append to `R/helpers.R`:

```r
# Internal: pool counts for a set of questions into a percentage composition,
# with ANS_TEXT ordered by `levels`. Returns NULL if no rows.
.context_compose <- function(context, questions, levels, exclude = character(0)) {
  if (is.null(context)) return(NULL)
  d <- context |>
    filter(QUES_TEXT %in% questions, !ANS_TEXT %in% exclude)
  if (nrow(d) == 0) return(NULL)
  d |>
    group_by(QUES_TEXT, ANS_TEXT) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(ANS_TEXT = factor(ANS_TEXT, levels = levels))
}

plot_interest_shift <- function(context) {
  qs <- c("My interest in subject before course", "My interest in subject after course")
  d <- .context_compose(context, qs, c("Low","Medium","High"))
  if (is.null(d)) return(context_empty_plot("Interest before vs after"))
  d <- d |>
    mutate(phase = factor(ifelse(grepl("before", QUES_TEXT), "Before", "After"),
                          levels = c("Before","After"))) |>
    group_by(phase) |> mutate(pct = 100 * count / sum(count)) |> ungroup()
  hi <- d |> filter(ANS_TEXT == "High")
  delta <- round(sum(hi$pct[hi$phase == "After"]) - sum(hi$pct[hi$phase == "Before"]), 0)
  ggplot2::ggplot(d, ggplot2::aes(phase, pct, fill = ANS_TEXT)) +
    ggplot2::geom_col() + ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "", y = "Percentage", fill = "Interest",
                  title = "Interest in the Subject, Before vs After",
                  subtitle = sprintf("Change in share at High interest: %+d points", delta))
}

plot_course_demands <- function(context) {
  qs <- c("Difficulty (relative to other courses)", "Workload (relative to other courses)")
  d <- .context_compose(context, qs, c("Low","Medium","High"))
  if (is.null(d)) return(context_empty_plot("Course demands"))
  d <- d |>
    mutate(item = factor(ifelse(grepl("Difficulty", QUES_TEXT), "Difficulty", "Workload"),
                         levels = c("Difficulty","Workload"))) |>
    group_by(item) |> mutate(pct = 100 * count / sum(count)) |> ungroup()
  ggplot2::ggplot(d, ggplot2::aes(item, pct, fill = ANS_TEXT)) +
    ggplot2::geom_col() + ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "", y = "Percentage", fill = "Relative to other courses",
                  title = "Course Demands: Difficulty and Workload")
}

plot_expected_grade <- function(context) {
  d <- .context_compose(context, "Expected Grade",
                        c("A","B","C","D","F","P","NP","Don't Know"))
  if (is.null(d)) return(context_empty_plot("Expected grades"))
  d <- d |> mutate(pct = 100 * count / sum(count))
  ggplot2::ggplot(d, ggplot2::aes(ANS_TEXT, pct)) +
    ggplot2::geom_col(fill = viridisLite::viridis(1, begin = 0.4)) +
    ggplot2::labs(x = "", y = "Percentage", title = "Expected Grade Distribution")
}

plot_material_usefulness <- function(context) {
  qs <- c("Usefulness of texts", "Usefulness of homework assignments",
          "Usefulness of lab assignments", "Usefulness of examinations",
          "Usefulness of class discussions")
  d <- .context_compose(context, qs, c("Low","Medium","High"), exclude = "Not Applicable")
  if (is.null(d)) return(context_empty_plot("Usefulness of course materials"))
  d <- d |>
    mutate(material = sub("^Usefulness of ", "", QUES_TEXT)) |>
    group_by(material) |> mutate(pct = 100 * count / sum(count)) |> ungroup()
  ggplot2::ggplot(d, ggplot2::aes(material, pct, fill = ANS_TEXT)) +
    ggplot2::geom_col() + ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "", y = "Percentage", fill = "Usefulness",
                  title = "Usefulness of Course Materials (rated students only)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0, size = 8))
}
```

- [ ] **Step 4: Run to verify pass** — expect PASS.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add four course-context plot helpers with empty-state handling"
```

---

## Task 7: Course Context tab + server wiring + report inclusion

**Files:** Modify `app.R` (UI tab; server context pipeline, outputs, downloads, report list)

- [ ] **Step 1: Add the "Course Context" nav_panel**

In `app.R`, immediately after the closing `)` of the `nav_panel("Reliability", ...)` and before the closing `)` of `navset_card_tab`, add:

```r
            ,
            nav_panel(
                "Course Context",
                card(card_header("Interest in the Subject, Before vs After"),
                     plotOutput("interest_plot"),
                     div(downloadButton("downloadInterestPlot", "Plot"),
                         downloadButton("downloadInterestData", "Data"))),
                card(card_header("Course Demands"),
                     plotOutput("demands_plot"),
                     div(downloadButton("downloadDemandsPlot", "Plot"),
                         downloadButton("downloadDemandsData", "Data"))),
                card(card_header("Expected Grades"),
                     plotOutput("grade_plot"),
                     div(downloadButton("downloadGradePlot", "Plot"),
                         downloadButton("downloadGradeData", "Data"))),
                card(card_header("Usefulness of Course Materials"),
                     plotOutput("usefulness_plot"),
                     div(downloadButton("downloadUsefulnessPlot", "Plot"),
                         downloadButton("downloadUsefulnessData", "Data")))
            )
```

- [ ] **Step 2: Build the context pipeline and plot objects in the server**

In `observeEvent`, immediately BEFORE the `output$downloadReport <- downloadHandler(` real-report block (app.R:384 area), add:

```r
        # Second pass: read the curated course-context questions from the same files.
        context_full <- NULL
        for (i in seq_len(nrow(work))) {
            cone <- tryCatch(read_context_file(work$path[i], work$name[i]), error = function(e) NULL)
            if (!is.null(cone)) context_full <- bind_rows(context_full, cone)
        }
        interest_plot   <- plot_interest_shift(context_full)
        demands_plot    <- plot_course_demands(context_full)
        grade_plot      <- plot_expected_grade(context_full)
        usefulness_plot <- plot_material_usefulness(context_full)

        output$interest_plot   <- renderPlot({ interest_plot })
        output$demands_plot    <- renderPlot({ demands_plot })
        output$grade_plot      <- renderPlot({ grade_plot })
        output$usefulness_plot <- renderPlot({ usefulness_plot })

        ctx_dl <- function(qs) function(file) {
            d <- if (is.null(context_full)) data.frame() else context_full[context_full$QUES_TEXT %in% qs, ]
            write.table(d, file, sep = ",", row.names = FALSE)
        }
        output$downloadInterestPlot <- downloadHandler("InterestBeforeAfter.png",
            content = function(file) ggsave(file, interest_plot, width = 7, height = 5, dpi = 300))
        output$downloadDemandsPlot <- downloadHandler("CourseDemands.png",
            content = function(file) ggsave(file, demands_plot, width = 7, height = 5, dpi = 300))
        output$downloadGradePlot <- downloadHandler("ExpectedGrades.png",
            content = function(file) ggsave(file, grade_plot, width = 7, height = 5, dpi = 300))
        output$downloadUsefulnessPlot <- downloadHandler("MaterialUsefulness.png",
            content = function(file) ggsave(file, usefulness_plot, width = 7, height = 5, dpi = 300))
        output$downloadInterestData <- downloadHandler("interest_before_after.csv",
            content = ctx_dl(c("My interest in subject before course","My interest in subject after course")))
        output$downloadDemandsData <- downloadHandler("course_demands.csv",
            content = ctx_dl(c("Difficulty (relative to other courses)","Workload (relative to other courses)")))
        output$downloadGradeData <- downloadHandler("expected_grades.csv",
            content = ctx_dl("Expected Grade"))
        output$downloadUsefulnessData <- downloadHandler("material_usefulness.csv",
            content = ctx_dl(c("Usefulness of texts","Usefulness of homework assignments",
                               "Usefulness of lab assignments","Usefulness of examinations",
                               "Usefulness of class discussions")))
```

- [ ] **Step 3: Append the context charts to the PDF report**

In the real `output$downloadReport` handler, the `report_plots <- list(...)` currently ends with `"Response Rate by Course" = response_rate_plot`. Add four entries to that list (before the closing `)`):

```r
                    ,
                    "Interest Before vs After" = interest_plot,
                    "Course Demands" = demands_plot,
                    "Expected Grades" = grade_plot,
                    "Usefulness of Course Materials" = usefulness_plot
```

- [ ] **Step 4: Verify parse, boot, report page count**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e 'invisible(parse("app.R")); cat("parses\n")'
Rscript -e 'shiny::runApp(".", port=7867, launch.browser=FALSE)' >/tmp/p2.log 2>&1 &
P=$!; sleep 8; curl -s -o /dev/null -w "HTTP %{http_code}\n" http://127.0.0.1:7867/ || echo fail
kill $P 2>/dev/null; grep -i "error\|listening" /tmp/p2.log | head
```
Expected: `parses`; HTTP 200; no R error. (The report now has 14 chart pages + cover when data is loaded.)

- [ ] **Step 5: Commit**

```bash
git add app.R
git commit -m "Add Course Context tab, context plots, downloads, and report pages"
```

---

## Task 8: Visual verification, end-to-end, README

**Files:** Modify `README.md`

- [ ] **Step 1: Unit suite green** — `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R` (0 failures).

- [ ] **Step 2: Visual check with Playwright**

Launch the app (`Rscript -e 'shiny::runApp(".", port=7868, launch.browser=FALSE)'` in background). With the Playwright browser tools: navigate to `http://127.0.0.1:7868/`, upload a zip built from the fixtures (`cd tests/testthat/fixtures && zip /tmp/ctx.zip ANTH243-01_FALL24.csv ANTH243-01_FALL19.csv ANTH482C-01_SPG25.csv TEST101-01_FALL25.xlsx`) via `page.setInputFiles('input[type=file]', '/tmp/ctx.zip')`, click "Process uploaded data", wait, then screenshot. Confirm the post-upload summary value boxes appear, and open the "Course Context" tab and confirm the four charts render. Report observations; if a chart is broken, STOP and report BLOCKED. Stop the app.

- [ ] **Step 3: Full end-to-end against the sample set**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs
rm -rf /tmp/ctx_e2e && mkdir /tmp/ctx_e2e && unzip -q sample-data/sample-data.zip -d /tmp/ctx_e2e
Rscript -e '
  suppressMessages({library(ggplot2); source("R/helpers.R")})
  files <- list.files("/tmp/ctx_e2e/SOOTs", pattern="\\.(csv|xlsx)$", full.names=TRUE)
  ctx <- NULL
  for (f in files) { one <- tryCatch(read_context_file(f, basename(f)), error=function(e) NULL); if(!is.null(one)) ctx <- dplyr::bind_rows(ctx, one) }
  cat("context rows:", nrow(ctx), " questions:", length(unique(ctx$QUES_TEXT)), "\n")
  for (p in list(plot_interest_shift(ctx), plot_course_demands(ctx), plot_expected_grade(ctx), plot_material_usefulness(ctx)))
    ggsave(tempfile(fileext=".png"), p, width=7, height=5)
  cat("CONTEXT E2E OK\n")
'
```
Expected: nonzero context rows across multiple questions; "CONTEXT E2E OK".

- [ ] **Step 4: Update README** — under the existing "What the app produces" / charts description, add a sentence: a Course Context tab now reports interest before vs after, course difficulty and workload, expected grades, and usefulness of course materials, drawn from questions present in both export formats. Note the post-upload summary and that uploads combining multiple instructors are flagged.

- [ ] **Step 5: Commit**

```bash
git add README.md
git commit -m "Document Course Context tab, upload summary, and multi-instructor warning"
```

---

## Self-review notes

- **Spec coverage:** post-upload summary (Tasks 1, 2), multi-instructor warning (Tasks 1, 2), cleanup + CI (Task 3), CONTEXT_QUESTIONS + decoding (Task 4), context readers CSV/XLSX/dispatch (Tasks 4, 5), four context charts with empty-state (Task 6), Course Context tab + downloads + report inclusion (Task 7), fixture extension (Task 5), tests throughout, visual/e2e/README (Task 8).
- **Type consistency:** `upload_summary` returns a list with `files_processed/files_skipped/n_courses/n_terms/respondents`; the server renderUI reads those. `distinct_instructors` returns a character vector. Context readers all return `tibble(course, term, QUES_TEXT, ANS_TEXT, count)`; the plot helpers and `.context_compose` consume those columns; `CONTEXT_TEXTS` is derived from `CONTEXT_QUESTIONS`. New output ids: upload_summary, interest_plot, demands_plot, grade_plot, usefulness_plot and their eight download ids. No instructor-pipeline names changed.
- **No regression:** the instructor canonical schema, `read_soot_file`, and all existing charts are untouched; the context pipeline is additive and read via a separate second pass.
- **Deployment (post-merge):** bundle unchanged (`app.R`, `R/helpers.R`, `www/BU-logo.png`); `app2.R` removed. No new runtime dependency.
