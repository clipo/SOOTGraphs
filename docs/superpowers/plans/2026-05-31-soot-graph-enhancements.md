# SOOT Graph Enhancements — Phase A Implementation Plan (CSV)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add four new graph types (six chart sections, including student- and course-weighted variants) plus light fixes to the existing stacked bars, all fed by a format-normalization layer that currently reads the old pre-aggregated SOOT CSV export.

**Architecture:** Extract all data logic out of `observeEvent` into pure, unit-tested functions in `R/helpers.R`. An ingestion layer converts each uploaded file to a single canonical long-format table; every chart consumes only that table. This plan covers Phase A (CSV); XLSX ingestion is a separate later plan that adds one reader to the same seam without touching downstream code.

**Tech Stack:** R, Shiny, tidyverse (dplyr/tidyr/ggplot2), viridisLite, testthat (unit tests). Phase B will add readxl.

**Scope note:** This is Phase A only. XLSX upload is rejected with a clear message until Phase B. Test fixtures are the aggregate-count CSVs only; the XLSX sample files contain instructor and student PII and must NOT be committed as fixtures.

---

## Canonical schema (the contract every chart depends on)

`read_soot_file()` returns a tibble with exactly these columns (one row per file x question x answer category):

| column | type | CSV source | notes |
|--------|------|-----------|-------|
| `course` | chr | filename element [1] | e.g. "ANTH243" |
| `term` | chr | filename element [3] | raw token, e.g. "FALL24", "SPG25" |
| `QUES_TEXT` | chr | column | full question text |
| `ANS_TEXT` | ordered factor | column | levels: Not Applicable < Very Low or Never < Low < Average < High < Very High or Always |
| `ANS_COUNT` | int | column | count of respondents in that category |
| `ANS_PCT` | num | column | percentage (kept for existing stacked charts) |
| `enrollment` | num | NA in Phase A | populated from XLSX in Phase B |
| `respondents` | num | NA in Phase A | |
| `response_rate` | num | NA in Phase A | |
| `instructor` | chr | NA in Phase A | |

The 6 factor levels are referenced repeatedly. Define them once in `R/helpers.R`:

```r
ANS_LEVELS <- c("Not Applicable", "Very Low or Never", "Low",
                "Average", "High", "Very High or Always")
TOP_BOX_LEVELS <- c("High", "Very High or Always")
```

The 9 instructor questions, in canonical order (already in `app.R`; move to `helpers.R` as a constant):

```r
INSTRUCTOR_QUESTIONS <- c(
  "The instructor is well prepared for class.",
  "The instructor demonstrates a thorough knowledge of the subject.",
  "The instructor communicates his/her subject well.",
  "The instructor explains complex ideas clearly.",
  "The instructor stimulates my interest in the core subject.",
  "The instructor is receptive to questions.",
  "The instructor is available to help me outside of class.",
  "The instructor encourages me to think analytically.",
  "Overall, the instructor is an effective teacher."
)
```

---

## File structure

- Create `R/helpers.R` — all pure data functions + the constants above.
- Create `tests/testthat.R` — test runner entry point.
- Create `tests/testthat/test-helpers.R` — unit tests for every helper.
- Create `tests/testthat/fixtures/` — a few real aggregate CSVs copied from the sample set (no PII).
- Modify `app.R` — source `R/helpers.R`; replace the file-reading loop and chart computations with helper calls; add the new UI sections and server outputs.
- Modify `README.md` — note the new graphs (final task).

---

## Task 1: Test infrastructure + helpers skeleton

**Files:**
- Create: `R/helpers.R`
- Create: `tests/testthat.R`
- Create: `tests/testthat/test-helpers.R`
- Create: `tests/testthat/fixtures/` (copy 3 CSVs)

- [ ] **Step 1: Create the helpers file with constants only**

Create `R/helpers.R`:

```r
# Pure data-transformation helpers for the SOOT aggregator.
# Sourced by app.R; unit-tested in tests/testthat/test-helpers.R.

library(dplyr)
library(tidyr)
library(stringr)

ANS_LEVELS <- c("Not Applicable", "Very Low or Never", "Low",
                "Average", "High", "Very High or Always")
TOP_BOX_LEVELS <- c("High", "Very High or Always")

INSTRUCTOR_QUESTIONS <- c(
  "The instructor is well prepared for class.",
  "The instructor demonstrates a thorough knowledge of the subject.",
  "The instructor communicates his/her subject well.",
  "The instructor explains complex ideas clearly.",
  "The instructor stimulates my interest in the core subject.",
  "The instructor is receptive to questions.",
  "The instructor is available to help me outside of class.",
  "The instructor encourages me to think analytically.",
  "Overall, the instructor is an effective teacher."
)
```

- [ ] **Step 2: Create test fixtures**

Run (copies three aggregate CSVs out of the sample zip; these are counts only, no PII):

```bash
mkdir -p tests/testthat/fixtures
cd /tmp && rm -rf fx && mkdir fx && unzip -q /Users/clipo/PycharmProjects/SOOTgraphs/sample-data/sample-data.zip -d fx
cp "fx/SOOTs/ANTH243-01_FALL24.csv" "fx/SOOTs/ANTH243-01_FALL19.csv" "fx/SOOTs/ANTH482C-01_SPG25.csv" \
   /Users/clipo/PycharmProjects/SOOTgraphs/tests/testthat/fixtures/
ls -la /Users/clipo/PycharmProjects/SOOTgraphs/tests/testthat/fixtures/
```

Expected: three .csv files listed, each ~6.5 KB.

- [ ] **Step 3: Create the test runner**

Create `tests/testthat.R` (the runner sets the working directory to `testthat/`
for each test file, so each test file sources helpers itself with `../../R`):

```r
library(testthat)
test_dir("testthat", reporter = "summary")
```

Create `tests/testthat/test-helpers.R` with a smoke test:

```r
source(file.path("..", "..", "R", "helpers.R"))

test_that("constants are defined", {
  expect_length(ANS_LEVELS, 6)
  expect_length(INSTRUCTOR_QUESTIONS, 9)
  expect_true(all(TOP_BOX_LEVELS %in% ANS_LEVELS))
})
```

- [ ] **Step 4: Run the tests**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS (1 test, 3 expectations). If `testthat` is missing, run `Rscript -e 'install.packages("testthat", repos="https://cloud.r-project.org")'` first.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/
git commit -m "Add helpers skeleton, constants, and testthat infrastructure"
```

---

## Task 2: parse_course_term()

**Files:**
- Modify: `R/helpers.R`
- Test: `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-helpers.R`:

```r
test_that("parse_course_term splits course and term from filename", {
  expect_equal(parse_course_term("ANTH243-01_FALL24.csv"),
               list(course = "ANTH243", term = "FALL24"))
  expect_equal(parse_course_term("ANTH482C-01_SPG25.csv"),
               list(course = "ANTH482C", term = "SPG25"))
  expect_equal(parse_course_term("ANTH243-90_SPG21.csv"),
               list(course = "ANTH243", term = "SPG21"))
})
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — "could not find function "parse_course_term"".

- [ ] **Step 3: Implement**

Append to `R/helpers.R`:

```r
# Split an uploaded filename into course and term tokens.
# Pattern: COURSE-SECTION_TERM.ext, split on "-", "_", or ".".
parse_course_term <- function(filename) {
  parts <- unlist(str_split(filename, "-|_|\\."))
  list(course = parts[1], term = parts[3])
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add parse_course_term helper"
```

---

## Task 3: order_terms()

**Files:**
- Modify: `R/helpers.R`
- Test: `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-helpers.R`:

```r
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
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — "could not find function "order_terms"".

- [ ] **Step 3: Implement**

Append to `R/helpers.R`:

```r
# Convert raw term tokens (e.g. "FALL24", "SPG25") into a chronological
# ordered factor. Sort key = year * 100 + season rank, spring before fall.
order_terms <- function(term_vector) {
  season_rank <- c(WIN = 0, SPG = 2, SPR = 2, SUM = 6, FALL = 9, FAL = 9)
  uniq <- unique(term_vector)
  season <- str_extract(uniq, "^[A-Za-z]+") |> toupper()
  year <- as.integer(str_extract(uniq, "[0-9]+$"))
  rank <- ifelse(season %in% names(season_rank), season_rank[season], 5)
  key <- year * 100 + as.integer(rank)
  ordered_levels <- uniq[order(key)]
  factor(term_vector, levels = ordered_levels, ordered = TRUE)
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add order_terms chronological helper"
```

---

## Task 4: read_soot_csv() and read_soot_file() dispatch

**Files:**
- Modify: `R/helpers.R`
- Test: `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-helpers.R`:

```r
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
  # Spot-check a known row
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
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — "could not find function "read_soot_file"".

- [ ] **Step 3: Implement**

Append to `R/helpers.R`:

```r
# Read one uploaded SOOT CSV into the canonical long format.
read_soot_csv <- function(path, name) {
  raw <- tryCatch(read.csv(path, header = TRUE, stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) {
    warning(sprintf("Skipping empty or unreadable file: %s", name))
    return(NULL)
  }
  ct <- parse_course_term(name)
  tibble::as_tibble(raw) |>
    mutate(course = ct$course,
           term = ct$term,
           ANS_TEXT = factor(ANS_TEXT, levels = ANS_LEVELS, ordered = TRUE),
           enrollment = NA_real_,
           respondents = NA_real_,
           response_rate = NA_real_,
           instructor = NA_character_) |>
    select(course, term, QUES_TEXT, ANS_TEXT, ANS_COUNT, ANS_PCT,
           enrollment, respondents, response_rate, instructor)
}

# Dispatch on file extension. XLSX support arrives in Phase B.
read_soot_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    read_soot_csv(path, name)
  } else if (ext %in% c("xlsx", "xls")) {
    stop("XLSX import not yet supported. Please upload the old-format CSV files for now.")
  } else {
    warning(sprintf("Skipping file with unrecognized extension: %s", name))
    NULL
  }
}
```

Add `library(tibble)` near the top of `R/helpers.R` with the other `library()` calls.

- [ ] **Step 4: Run to verify it passes**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS (all read_soot_file tests).

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add CSV reader and file dispatch with empty-file and XLSX handling"
```

---

## Task 5: compute_top_two_box() — both weightings

**Files:**
- Modify: `R/helpers.R`
- Test: `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-helpers.R`. The inline data has two courses of different size so the two weightings give different answers (student-weighted = 33.33%, course-weighted = 45%):

```r
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
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — "could not find function "compute_top_two_box"".

- [ ] **Step 3: Implement**

Append to `R/helpers.R`:

```r
# Per-course-per-group top-two-box from raw counts. Internal helper.
.top_two_box_by_course <- function(data, group_vars) {
  data |>
    group_by(across(all_of(c("course", group_vars)))) |>
    summarise(
      non_na = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]),
      top    = sum(ANS_COUNT[ANS_TEXT %in% TOP_BOX_LEVELS]),
      .groups = "drop"
    ) |>
    mutate(course_ttb = ifelse(non_na > 0, 100 * top / non_na, NA_real_))
}

# Top-two-box percentage per group, by weighting method.
#   weighting = "student": pool all counts, then compute the percentage.
#   weighting = "course":  per-course percentage, averaged equally across courses.
# Returns one row per group with: top_two_box, plus n (student) or n_courses (course).
compute_top_two_box <- function(data, group_vars, weighting = c("student", "course")) {
  weighting <- match.arg(weighting)
  if (weighting == "student") {
    data |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        n = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]),
        top = sum(ANS_COUNT[ANS_TEXT %in% TOP_BOX_LEVELS]),
        .groups = "drop"
      ) |>
      mutate(top_two_box = ifelse(n > 0, 100 * top / n, NA_real_)) |>
      select(all_of(group_vars), top_two_box, n)
  } else {
    .top_two_box_by_course(data, group_vars) |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        top_two_box = mean(course_ttb, na.rm = TRUE),
        n_courses = sum(!is.na(course_ttb)),
        .groups = "drop"
      ) |>
      mutate(top_two_box = ifelse(is.nan(top_two_box), NA_real_, top_two_box))
  }
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add compute_top_two_box with student- and course-weighting"
```

---

## Task 6: compute_rating_distribution() — N/A excluded

**Files:**
- Modify: `R/helpers.R`
- Test: `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-helpers.R`:

```r
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
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — "could not find function "compute_rating_distribution"".

- [ ] **Step 3: Implement**

Append to `R/helpers.R`:

```r
# Rating distribution per group, computed from counts EXCLUDING "Not Applicable",
# so the colored bars reflect only students who rated. The N/A share is reported
# separately. Returns a list(dist = <per-answer percentages>, na_share = <per-group>).
compute_rating_distribution <- function(data, group_vars) {
  totals <- data |>
    group_by(across(all_of(group_vars))) |>
    summarise(total_all = sum(ANS_COUNT),
              na_count = sum(ANS_COUNT[ANS_TEXT == "Not Applicable"]),
              non_na = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]),
              .groups = "drop") |>
    mutate(na_pct = ifelse(total_all > 0, 100 * na_count / total_all, NA_real_))

  dist <- data |>
    filter(ANS_TEXT != "Not Applicable") |>
    group_by(across(all_of(c(group_vars, "ANS_TEXT")))) |>
    summarise(count = sum(ANS_COUNT), .groups = "drop") |>
    left_join(totals |> select(all_of(group_vars), non_na), by = group_vars) |>
    mutate(pct = ifelse(non_na > 0, 100 * count / non_na, NA_real_),
           ANS_TEXT = droplevels(factor(ANS_TEXT, levels = ANS_LEVELS, ordered = TRUE)))

  list(dist = dist,
       na_share = totals |> select(all_of(group_vars), na_count, na_pct))
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add compute_rating_distribution with N/A separation"
```

---

## Task 7: Wire helpers into app.R ingestion

**Files:**
- Modify: `app.R:1-7` (add source), `app.R` server file-reading block (currently lines ~89-130)

- [ ] **Step 1: Source helpers at the top of app.R**

After the `library(...)` lines at the top of `app.R`, add:

```r
source("R/helpers.R")
```

- [ ] **Step 2: Replace the ingestion block**

Replace the current block that begins `files <- input$csvs$datapath` and ends at the closing of the `full` assembly loop (the loop that `rbind`s `read.csv(files[i], ...)`), with:

```r
        # Read every uploaded file through the ingestion layer into the
        # canonical long format. Empty/unsupported files are skipped (NULL).
        full <- NULL
        for (i in seq_along(input$csvs$datapath)) {
            one <- tryCatch(
                read_soot_file(input$csvs$datapath[i], input$csvs$name[i]),
                error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
            )
            if (!is.null(one)) full <- bind_rows(full, one)
        }
        if (is.null(full) || nrow(full) == 0) {
            showNotification("No usable data found in the uploaded files.", type = "warning")
            return()
        }
```

- [ ] **Step 3: Keep the instructor-question filter, drop the old term munging**

The existing `instructor_related_ques` assignment filters to the 9 questions and then does `mutate(fall.spring = ...) ... unite(term, ...)`. Replace that block with a filter that keeps `term` as the raw token and uses the constant:

```r
        instructor_related_ques <- full |>
            filter(QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
```

(The chronological ordering is now applied at plot time via `order_terms()`, not by rewriting `term`.)

- [ ] **Step 4: Smoke-test the app loads and processes**

Run:

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e '
  source("R/helpers.R")
  files <- list.files("tests/testthat/fixtures", full.names = TRUE)
  full <- do.call(dplyr::bind_rows, lapply(files, function(f) read_soot_file(f, basename(f))))
  cat("rows:", nrow(full), " courses:", length(unique(full$course)),
      " terms:", paste(unique(full$term), collapse=","), "\n")
  irq <- dplyr::filter(full, QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
  cat("instructor-question rows:", nrow(irq), "\n")
'
```

Expected: prints nonzero rows, 2 courses (ANTH243, ANTH482C), terms FALL24,FALL19,SPG25, and instructor-question rows = 9 questions x 6 answers x 3 files = 162.

- [ ] **Step 5: Commit**

```bash
git add app.R
git commit -m "Wire ingestion layer into app; keep raw term token"
```

---

## Task 8: Light fixes to the existing stacked charts

**Files:**
- Modify: `app.R` (the `percentages_overall`, `percentages_by_term`, `percentages_by_course` blocks and their plots)

- [ ] **Step 1: Rebuild the three distributions from the helper**

Replace each of the three `percentages_*` computations (which currently use `left_join(ques_count)` and `sum(ANS_PCT*ques_count)/sum(...)`) with calls to `compute_rating_distribution`:

```r
        overall_dist <- compute_rating_distribution(instructor_related_ques, "QUES_TEXT")
        percentages_overall <- overall_dist$dist |>
            rename(Answer = ANS_TEXT, mean_PCT = pct) |>
            mutate(QUES_TEXT = factor(QUES_TEXT, levels = INSTRUCTOR_QUESTIONS))

        term_dist <- compute_rating_distribution(instructor_related_ques,
                                                 c("QUES_TEXT", "term"))
        percentages_by_term <- term_dist$dist |>
            rename(Answer = ANS_TEXT, mean_PCT = pct) |>
            mutate(term = order_terms(term))

        course_dist <- compute_rating_distribution(instructor_related_ques,
                                                   c("QUES_TEXT", "course"))
        percentages_by_course <- course_dist$dist |>
            rename(Answer = ANS_TEXT, mean_PCT = pct)
```

Keep the existing `swr` string-wrap of `QUES_TEXT` for the faceted term/course plots, applied after the above.

- [ ] **Step 2: Order the term axis and annotate N/A**

In the `term_plot` ggplot, `term` is already an ordered factor from `order_terms`, so the x-axis sorts chronologically with no other change. Add an N/A note to the overall plot subtitle. Replace the overall plot's `labs(... subtitle = ...)` with:

```r
        na_overall <- round(mean(overall_dist$na_share$na_pct, na.rm = TRUE), 1)
        overall_plot <- overall_plot + labs(
            title = "Overall Results Aggregated by Question",
            subtitle = sprintf(
                "Generated by the Harpur College SOOT Aggregator. Bars show ratings only; 'Not Applicable' averaged %.1f%% of responses and is excluded.",
                na_overall))
```

- [ ] **Step 3: Smoke-test the distributions sum to 100 among rated**

Run:

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e '
  source("R/helpers.R")
  files <- list.files("tests/testthat/fixtures", full.names = TRUE)
  full <- do.call(dplyr::bind_rows, lapply(files, function(f) read_soot_file(f, basename(f))))
  irq <- dplyr::filter(full, QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
  d <- compute_rating_distribution(irq, "QUES_TEXT")$dist
  chk <- dplyr::summarise(dplyr::group_by(d, QUES_TEXT), s = round(sum(pct),2))
  print(chk); stopifnot(all(abs(chk$s - 100) < 0.01))
  cat("all questions sum to 100 among rated\n")
'
```

Expected: every question's percentages sum to 100; prints the confirmation line.

- [ ] **Step 4: Commit**

```bash
git add app.R
git commit -m "Rebuild existing stacked charts from helper; exclude N/A; order terms"
```

---

## Task 9: Summary-by-question charts (1a student, 1b course) + explanatory text

**Files:**
- Modify: `app.R` UI (add two sections + explanatory `p()` + download buttons) and server (two plots + two CSV downloads)

- [ ] **Step 1: Add a reusable plot function to helpers**

Append to `R/helpers.R`:

```r
library(ggplot2)

# Horizontal bar of top-two-box % per question, in canonical order.
plot_summary_by_question <- function(ttb, weighting_label, n_label_col) {
  ttb <- ttb |>
    mutate(QUES_TEXT = factor(QUES_TEXT, levels = rev(INSTRUCTOR_QUESTIONS)),
           lbl = ifelse(is.na(top_two_box), "n/a",
                        sprintf("%.0f%% (%s=%d)", top_two_box,
                                ifelse("n" %in% names(ttb), "n", "courses"),
                                .data[[n_label_col]])))
  ggplot(ttb, aes(x = QUES_TEXT, y = top_two_box)) +
    geom_col(fill = viridisLite::viridis(1, begin = 0.4)) +
    geom_text(aes(label = lbl), hjust = -0.05, size = 3) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 109), breaks = seq(0, 100, 25)) +
    labs(x = "", y = "Top-two-box % (High or Very High)",
         title = sprintf("Summary Score by Question (%s)", weighting_label)) +
    theme(axis.text.y = element_text(size = 9))
}
```

- [ ] **Step 2: Add the two UI sections (after the existing course section, before Course Inventory)**

```r
    h2("Summary Score by Question"),
    fluidRow(plotOutput("summary_student_plot")),
    fluidRow(
        downloadButton("downloadSummaryStudentPlot", "Download Student-Weighted Plot"),
        downloadButton("downloadSummaryStudentData", "Download Student-Weighted Data")
    ),
    fluidRow(plotOutput("summary_course_plot")),
    fluidRow(
        downloadButton("downloadSummaryCoursePlot", "Download Course-Weighted Plot"),
        downloadButton("downloadSummaryCourseData", "Download Course-Weighted Data")
    ),
    p("Two summaries of the same ratings are shown. The student-weighted version pools
      every student response together, so courses with more respondents have more
      influence; it reflects the experience of the average student. The course-weighted
      version summarizes each course on its own and then averages those course summaries
      equally, so every course counts the same regardless of size; it reflects the typical
      course. The two differ when class sizes vary: a single large course pulls the
      student-weighted number toward its own ratings, while the course-weighted number
      gives a small seminar the same say as a large lecture."),
```

- [ ] **Step 3: Add the server outputs**

Inside `observeEvent`, after the existing course outputs:

```r
        ttb_student <- compute_top_two_box(instructor_related_ques, "QUES_TEXT", "student")
        ttb_course  <- compute_top_two_box(instructor_related_ques, "QUES_TEXT", "course")

        summary_student_plot <- plot_summary_by_question(ttb_student, "Student-Weighted", "n")
        summary_course_plot  <- plot_summary_by_question(ttb_course, "Course-Weighted", "n_courses")

        output$summary_student_plot <- renderPlot({ summary_student_plot })
        output$summary_course_plot  <- renderPlot({ summary_course_plot })

        output$downloadSummaryStudentPlot <- downloadHandler(
            filename = "SummaryByQuestion_StudentWeighted.png",
            content = function(file) { ggsave(file, summary_student_plot, width = 7, height = 5, dpi = 300) })
        output$downloadSummaryCoursePlot <- downloadHandler(
            filename = "SummaryByQuestion_CourseWeighted.png",
            content = function(file) { ggsave(file, summary_course_plot, width = 7, height = 5, dpi = 300) })
        output$downloadSummaryStudentData <- downloadHandler(
            filename = "summary_by_question_student.csv",
            content = function(file) { write.table(ttb_student, file, sep = ",", row.names = FALSE) })
        output$downloadSummaryCourseData <- downloadHandler(
            filename = "summary_by_question_course.csv",
            content = function(file) { write.table(ttb_course, file, sep = ",", row.names = FALSE) })
```

- [ ] **Step 4: Manual visual check**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e 'shiny::runApp(".", launch.browser = FALSE, port = 7777)'` in the background, then upload the three fixture CSVs and click Process. Confirm both summary charts render, with course-weighted differing from student-weighted, and the explanatory paragraph appears below them. (Or use the `run`/`verify` skill to drive a browser.)

- [ ] **Step 5: Commit**

```bash
git add app.R R/helpers.R
git commit -m "Add student- and course-weighted summary-by-question charts with explanatory text"
```

---

## Task 10: Trend-by-term charts (2a student, 2b course)

**Files:**
- Modify: `R/helpers.R` (trend plot fn), `app.R` (UI + server)

- [ ] **Step 1: Add the trend plot function**

Append to `R/helpers.R`:

```r
# Small-multiple line of top-two-box % across terms, one panel per question.
plot_trends <- function(ttb_by_term, weighting_label) {
  ttb_by_term <- ttb_by_term |>
    mutate(term = order_terms(term),
           QUES_TEXT = factor(QUES_TEXT, levels = INSTRUCTOR_QUESTIONS))
  ggplot(ttb_by_term, aes(x = term, y = top_two_box, group = 1)) +
    geom_line(color = viridisLite::viridis(1, begin = 0.4)) +
    geom_point(color = viridisLite::viridis(1, begin = 0.4)) +
    facet_wrap(~QUES_TEXT, labeller = label_wrap_gen(width = 25)) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "", y = "Top-two-box %",
         title = sprintf("Summary Score Trends by Term (%s)", weighting_label)) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 7),
          strip.text = element_text(size = 7))
}
```

- [ ] **Step 2: Add the two UI sections (after the summary sections)**

```r
    h2("Summary Score Trends by Term"),
    fluidRow(plotOutput("trend_student_plot", height = "600px")),
    fluidRow(
        downloadButton("downloadTrendStudentPlot", "Download Student-Weighted Trend"),
        downloadButton("downloadTrendStudentData", "Download Student-Weighted Trend Data")
    ),
    fluidRow(plotOutput("trend_course_plot", height = "600px")),
    fluidRow(
        downloadButton("downloadTrendCoursePlot", "Download Course-Weighted Trend"),
        downloadButton("downloadTrendCourseData", "Download Course-Weighted Trend Data")
    ),
```

- [ ] **Step 3: Add the server outputs**

```r
        trend_student <- compute_top_two_box(instructor_related_ques, c("QUES_TEXT","term"), "student")
        trend_course  <- compute_top_two_box(instructor_related_ques, c("QUES_TEXT","term"), "course")

        trend_student_plot <- plot_trends(trend_student, "Student-Weighted")
        trend_course_plot  <- plot_trends(trend_course, "Course-Weighted")

        output$trend_student_plot <- renderPlot({ trend_student_plot })
        output$trend_course_plot  <- renderPlot({ trend_course_plot })

        output$downloadTrendStudentPlot <- downloadHandler(
            filename = "Trends_StudentWeighted.png",
            content = function(file) { ggsave(file, trend_student_plot, width = 9, height = 6, dpi = 300) })
        output$downloadTrendCoursePlot <- downloadHandler(
            filename = "Trends_CourseWeighted.png",
            content = function(file) { ggsave(file, trend_course_plot, width = 9, height = 6, dpi = 300) })
        output$downloadTrendStudentData <- downloadHandler(
            filename = "trends_student.csv",
            content = function(file) { write.table(trend_student, file, sep = ",", row.names = FALSE) })
        output$downloadTrendCourseData <- downloadHandler(
            filename = "trends_course.csv",
            content = function(file) { write.table(trend_course, file, sep = ",", row.names = FALSE) })
```

- [ ] **Step 4: Manual visual check**

Re-run the app with the fixtures (ANTH243 has two terms: FALL19, FALL24). Confirm each panel shows a line connecting the two terms in chronological order; single-term courses show isolated points.

- [ ] **Step 5: Commit**

```bash
git add app.R R/helpers.R
git commit -m "Add student- and course-weighted trend-by-term charts"
```

---

## Task 11: Response-counts-by-course chart

**Files:**
- Modify: `R/helpers.R` (plot fn), `app.R` (UI + server)

- [ ] **Step 1: Add the response-count plot function**

Append to `R/helpers.R`:

```r
# Bar of rating respondents per course (max over questions), grouped by term.
plot_response_counts <- function(data) {
  counts <- data |>
    group_by(course, term, QUES_TEXT) |>
    summarise(n = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]), .groups = "drop") |>
    group_by(course, term) |>
    summarise(respondents = max(n), .groups = "drop") |>
    mutate(term = order_terms(term))
  ggplot(counts, aes(x = course, y = respondents, fill = term)) +
    geom_col(position = position_dodge(preserve = "single")) +
    scale_fill_viridis_d() +
    labs(x = "", y = "Rating respondents", fill = "Term",
         title = "Response Counts by Course") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 8))
}
```

- [ ] **Step 2: Add the UI section**

```r
    h2("Response Counts by Course"),
    p("Number of rating respondents per course. Percentages from courses with few
      respondents are less reliable; use this to judge how much weight to give each bar."),
    fluidRow(plotOutput("response_count_plot")),
    fluidRow(
        downloadButton("downloadResponseCountPlot", "Download Response Count Plot"),
        downloadButton("downloadResponseCountData", "Download Response Count Data")
    ),
```

- [ ] **Step 3: Add the server outputs**

```r
        response_count_plot <- plot_response_counts(instructor_related_ques)
        response_count_data <- instructor_related_ques |>
            group_by(course, term, QUES_TEXT) |>
            summarise(n = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]), .groups = "drop") |>
            group_by(course, term) |>
            summarise(respondents = max(n), .groups = "drop")

        output$response_count_plot <- renderPlot({ response_count_plot })
        output$downloadResponseCountPlot <- downloadHandler(
            filename = "ResponseCountsByCourse.png",
            content = function(file) { ggsave(file, response_count_plot, width = 7, height = 5, dpi = 300) })
        output$downloadResponseCountData <- downloadHandler(
            filename = "response_counts_by_course.csv",
            content = function(file) { write.table(response_count_data, file, sep = ",", row.names = FALSE) })
```

- [ ] **Step 4: Manual visual check**

Re-run with fixtures; confirm bars for ANTH243 (two terms) and ANTH482C (one term), heights matching respondent counts.

- [ ] **Step 5: Commit**

```bash
git add app.R R/helpers.R
git commit -m "Add response-counts-by-course reliability chart"
```

---

## Task 12: Question-by-course heatmap

**Files:**
- Modify: `R/helpers.R` (plot fn), `app.R` (UI + server)

- [ ] **Step 1: Add the heatmap plot function**

Append to `R/helpers.R`:

```r
# Heatmap of top-two-box % with questions (rows) by course (columns).
plot_question_course_heatmap <- function(data) {
  ttb <- compute_top_two_box(data, c("QUES_TEXT", "course"), "student") |>
    mutate(QUES_TEXT = factor(QUES_TEXT, levels = rev(INSTRUCTOR_QUESTIONS)))
  ggplot(ttb, aes(x = course, y = QUES_TEXT, fill = top_two_box)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(is.na(top_two_box), "", sprintf("%.0f", top_two_box))),
              size = 3, color = "white") +
    scale_fill_viridis_c(limits = c(0, 100), name = "Top-two-box %") +
    labs(x = "", y = "", title = "Top-Two-Box % by Question and Course") +
    theme(axis.text.y = element_text(size = 8),
          axis.text.x = element_text(angle = -45, hjust = 0, size = 8))
}
```

- [ ] **Step 2: Add the UI section**

```r
    h2("Question-by-Course Comparison"),
    p("Top-two-box percentage for each question in each course. Use it to see whether a
      weaker dimension is specific to one course or consistent across courses."),
    fluidRow(plotOutput("heatmap_plot", height = "500px")),
    fluidRow(
        downloadButton("downloadHeatmapPlot", "Download Heatmap"),
        downloadButton("downloadHeatmapData", "Download Heatmap Data")
    ),
```

- [ ] **Step 3: Add the server outputs**

```r
        heatmap_plot <- plot_question_course_heatmap(instructor_related_ques)
        heatmap_data <- compute_top_two_box(instructor_related_ques, c("QUES_TEXT","course"), "student")

        output$heatmap_plot <- renderPlot({ heatmap_plot })
        output$downloadHeatmapPlot <- downloadHandler(
            filename = "QuestionByCourseHeatmap.png",
            content = function(file) { ggsave(file, heatmap_plot, width = 8, height = 5, dpi = 300) })
        output$downloadHeatmapData <- downloadHandler(
            filename = "question_by_course.csv",
            content = function(file) { write.table(heatmap_data, file, sep = ",", row.names = FALSE) })
```

- [ ] **Step 4: Manual visual check**

Re-run with fixtures; confirm a 9-row (questions) by 2-column (ANTH243, ANTH482C) tile grid with viridis fill and numeric labels.

- [ ] **Step 5: Commit**

```bash
git add app.R R/helpers.R
git commit -m "Add question-by-course top-two-box heatmap"
```

---

## Task 13: Full end-to-end verification and README

**Files:**
- Modify: `README.md`

- [ ] **Step 1: Run the whole test suite**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: all tests PASS, 0 failures.

- [ ] **Step 2: Full app run against the complete sample set**

Extract the full sample zip to a temp dir, launch the app, upload all CSVs from `sample-data` (including the 3 empty files), click Process. Confirm: the empty files trigger a skip warning but processing continues; all existing charts plus the six new sections render; downloads produce non-empty correct CSVs. Document any rendering issue and fix before continuing.

- [ ] **Step 3: Update README**

Add a short paragraph to `README.md` listing the new outputs (summary score by question in two weightings, trends by term in two weightings, response counts by course, question-by-course heatmap) and noting that XLSX import is planned next.

- [ ] **Step 4: Commit**

```bash
git add README.md
git commit -m "Document new SOOT graphs in README"
```

---

## Self-review notes

- **Spec coverage:** ingestion layer (Tasks 4, 7), canonical schema (Task 4), top-two-box both weightings (Task 5), N/A separation + chronological terms (Tasks 3, 8), six new charts (Tasks 9-12), explanatory text below summary charts (Task 9, Step 2), empty-file handling (Task 4), edge cases all-NA/single-term/single-course (helpers return NA; plots degrade), testability via helpers (all tasks). XLSX rejection in Phase A (Task 4).
- **Out of scope here:** XLSX reader, response-rate chart, course/self-assessment questions — all Phase B.
- **PII:** only aggregate CSVs used as fixtures; XLSX sample files (instructor names, student IPs) never committed.
- **Type consistency:** `compute_top_two_box` returns `top_two_box` + (`n` | `n_courses`); plot/download code references those exact names. `compute_rating_distribution` returns `list(dist, na_share)`; consumers use `$dist`/`$na_share`. `order_terms` returns an ordered factor used directly as a ggplot x aesthetic.
