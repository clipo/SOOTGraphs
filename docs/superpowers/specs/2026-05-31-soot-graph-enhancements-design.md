# SOOT Graph Enhancements — Design Spec

Date: 2026-05-31
Status: Approved for planning
Project: SOOTGraphs (Harpur College SOOT Aggregator Shiny app)

## Purpose

The app aggregates Binghamton University SOOT (Student Opinion of Teaching) data
for an instructor across courses and terms and produces graphs. Today every graph
is the same chart type: a 100%-stacked bar of six answer categories, faceted by
question, re-grouped overall / by term / by course. The app never reduces a
question to a single comparable number, never shows a chronological trend, folds
"Not Applicable" into the rating stack, and ignores response volume.

This work adds four new graphs, applies light fixes to the existing charts, and
introduces a format-normalization layer so the app can consume both the old
pre-aggregated CSV export and the new per-respondent XLSX export.

## Confirmed data schemas

### Old CSV (pre-aggregated)
Columns: `QUES, QUES_TEXT, ANS_TEXT, ANS_COUNT, ANS_PCT`. One row per
question x answer-category. 22 questions total: the 9 instructor items plus
course/self-assessment items the app currently discards. `course` and `term` are
parsed from the filename (`COURSE-SECTION_TERM.csv`, split on `-|_|\.`, element
[1] = course, [3] = term). Some exported files are 0 bytes. No instructor name,
enrollment, or response rate.

### New XLSX (per-respondent raw)
Two sheets:
- `RawData`: one row per respondent. 21 metadata columns
  (`ResponseUniqueId` ... `SubmitDevice`) including `CourseCode`, `CourseTitle`,
  `InstructorName`, `Enrollments`, `Respondents`, `ResponseRate`, followed by
  `Question 1` ... `Question 23` holding numeric answer codes.
- `QuestionMapper`: maps each `Question N` column to its full question text.

Answer code mapping (rating questions):

| code | label |
|------|-------|
| 0 | Not Applicable |
| 1 | Very Low or Never |
| 2 | Low |
| 3 | Average |
| 4 | High |
| 5 | Very High or Always |
| blank / None | no response — dropped, NOT equivalent to code 0 |

Questions 1-9 are the instructor items. Their mapper text ends with the exact CSV
question string after the final `" - "` separator
(e.g. `[InstructorName]-Instructor Effectiveness and Teaching Practices - The
instructor is well prepared for class.`), so they align to the CSV `QUES_TEXT`
after stripping the prefix. Questions 10-23 are course/self-assessment items
(out of scope for now).

## Core metric

Each question is reduced to a single summary score: **top-two-box percentage** =

```
top_two_box = 100 * (count of "High" + count of "Very High or Always")
                   / (count of all non-"Not Applicable" ratings)
```

All summaries are derived from `ANS_COUNT` (raw counts), not the pre-computed
`ANS_PCT`. Counts are the correct substrate: summing them across courses/terms
yields a properly response-weighted pooled estimate and clean N/A handling in one
place.

Two pooling methods are both reported, because instructors are evaluated on both
scales:

- **Student-weighted:** pool all `ANS_COUNT` across courses, then compute
  top-two-box. A 50-respondent course contributes more than a 5-respondent course.
  Answers "how did the average student rate this instructor."
- **Course-weighted:** compute top-two-box per course first, then take the
  unweighted mean across courses. Each course counts equally regardless of size.
  Answers "how did the average course go," so a single large intro course does not
  dominate.

Both are computed wherever pooling across courses occurs (the summary-by-question
and trend charts) and presented as separate charts. Charts where no cross-course
pooling happens (per-course bars, the heatmap, the existing stacked distributions)
are unaffected.

## Architecture

### Ingestion / normalization layer (the central new abstraction)

Both formats are converted up front to a single canonical long-format table.
Every existing and new graph consumes only this table and is format-agnostic.

Canonical row:

```
course, term, QUES_TEXT, ANS_TEXT (6-level ordered factor), ANS_COUNT, ANS_PCT,
enrollment, respondents, response_rate, instructor   # last four NA for old CSV
```

- **CSV -> canonical:** near pass-through. Read the file, attach `course`/`term`
  from the filename, set the four metadata columns to NA.
- **XLSX -> canonical:** read `RawData` and `QuestionMapper`; melt the `Question N`
  columns to one row per respondent x question; join the mapper; strip the prefix
  to recover `QUES_TEXT`; map codes 0-5 to labels, dropping blanks and keeping 0 as
  "Not Applicable"; aggregate to `ANS_COUNT` (n per question x answer) and
  `ANS_PCT` (count / non-missing total * 100); attach `course`/`term` from the
  filename and the metadata columns from `RawData`.

The 6-level ordered factor levels are, in order:
`Not Applicable, Very Low or Never, Low, Average, High, Very High or Always`.

### Code structure and testability

All aggregation logic currently lives inside `observeEvent`, making it untestable.
Extract pure functions into `R/helpers.R`, sourced by `app.R`:

- `read_soot_file(path, name)` — dispatch on extension, return canonical rows for
  one uploaded file (or NULL with a warning for empty/unreadable files).
- `read_soot_csv(path, name)` — CSV reader.
- `read_soot_xlsx(path, name)` — XLSX reader (Phase B).
- `parse_course_term(filename)` — filename -> list(course, term).
- `order_terms(term_vector)` — return terms as a chronological ordered factor.
- `compute_top_two_box(canonical, group_vars, weighting = "student")` — top-two-box
  % + rating-respondent n per group. `weighting = "student"` pools counts across
  courses; `weighting = "course"` computes per-course top-two-box and averages it
  equally across courses within each group.
- `compute_rating_distribution(canonical, group_vars)` — per-group answer
  distribution computed from counts excluding "Not Applicable", with the N/A share
  reported separately.

`app.R` rewires the existing charts to call these helpers. Unit tests live in
`tests/testthat/` and check each helper against hand-calculated values using the
real files in `sample-data/`.

## Graphs

### Six new graphs
Each is appended as its own UI section with `plotOutput` + a PNG download + a CSV
download, matching the existing pattern. The two summary charts and the two trend
charts come in student-weighted and course-weighted variants, presented as separate
sections (per the both-scales requirement above).

1a. **Summary score by question (student-weighted)** — horizontal bar, 9 questions,
   x = top-two-box %, each bar annotated with its rating-respondent n. Questions in
   canonical SOOT order so "Overall, the instructor is an effective teacher." is
   always findable and comparable across instructors. Pooled across all uploaded
   courses/terms, student-weighted.

1b. **Summary score by question (course-weighted)** — identical layout, but each
   course's top-two-box is averaged equally across courses. Annotated with the
   number of courses contributing.

2a. **Summary trends by term (student-weighted)** — small multiples, one panel per
   question, line of student-weighted top-two-box % across terms in chronological
   order, y fixed 0-100. With a single term it degrades to a single labeled point.

2b. **Summary trends by term (course-weighted)** — identical layout, course-weighted
   within each term.

An explanatory paragraph is shown in the UI directly below the summary-by-question
charts (static `p()` text, always visible), so a viewer understands why two versions
exist. Exact copy:

> Two summaries of the same ratings are shown. The student-weighted version pools
> every student response together, so courses with more respondents have more
> influence; it reflects the experience of the average student. The course-weighted
> version summarizes each course on its own and then averages those course
> summaries equally, so every course counts the same regardless of size; it
> reflects the typical course. The two differ when class sizes vary: a single large
> course pulls the student-weighted number toward its own ratings, while the
> course-weighted number gives a small seminar the same say as a large lecture.

3. **Response counts by course** — bar of rating respondents per course, grouped by
   term. The reliability layer: surfaces which courses' percentages rest on small
   samples. Also plots the `responses_by_course` table currently offered only as a
   CSV download.

4. **Question-by-course heatmap** — tiles, x = course, y = question, fill =
   top-two-box % (viridis). Shows whether a weak dimension is course-specific or
   pervasive. Degrades gracefully with a single course.

### Light fixes to existing stacked bars (look preserved)
Applies to the overall / by-term / by-course stacked charts:
- Recompute the stack from counts **excluding "Not Applicable"**, so the colored
  bar reflects only students who rated; report the N/A share separately in the
  subtitle.
- Order term panels/axes chronologically using the parsed year + semester.

The familiar stacked-bar appearance is otherwise unchanged.

## Figure style
Follows shared research conventions: sans-serif font, colorblind-friendly palette
(viridis, already in use), clean minimal design, subtle or absent gridlines. The
app's on-screen titles/subtitles are retained as currently deployed; downloaded
PNGs follow the same styling.

## Phasing

### Phase A — now (CSV only)
- Ingestion layer with the CSV reader + canonical schema.
- `R/helpers.R` pure functions + `tests/testthat/` tests.
- The four new graphs.
- Light fixes to the existing charts.
- Tested end to end against the real CSVs in `sample-data/` (including the 3 empty
  files, which are skipped with a warning).
- XLSX upload is rejected with a clear "XLSX import not yet supported" message.

### Phase B — next (XLSX)
- Add the XLSX reader to the ingestion layer; nothing downstream changes.
- Add `readxl` to the deployed package set (packrat / manifest for shinyapps.io).
- Because XLSX carries `Enrollments` and `Respondents`, add a fifth graph,
  **response rate by course**, which renders for XLSX-sourced courses and shows
  "not available" for CSV-sourced ones.

## Edge cases
- Zero files uploaded — already guarded (warning notification, early return).
- Empty (0-byte) files — skipped with a per-file warning; processing continues.
- Mixed CSV + XLSX in one upload batch — handled per file by the ingestion layer
  (Phase B); in Phase A a mixed batch yields the "XLSX not yet supported" message.
- Blank XLSX response vs. code-0 "Not Applicable" — blanks dropped, code 0 kept.
- All-N/A or zero-rating question (division by zero) — reported as NA with a note,
  no crash.
- Single term — trends graph degrades to points.
- Single course — heatmap degrades gracefully.

## Out of scope (future work)
- Course-level and self-assessment questions (10-23): workload, expected grade,
  interest before/after, etc. Present in both formats but not surfaced.
- Mean-on-1-5 or net-favorable summary metrics (top-two-box chosen).
- Multi-instructor handling using XLSX `InstructorName`.

## Testing approach
Unit tests for every helper against hand-calculated expected values, using the
real sample files. End-to-end check: load the full `sample-data/` CSV set in the
running app and confirm each graph renders and each download produces correct data.
No synthetic fixture needed; real data covers the cases (multiple courses, multiple
terms, empty files, varied response volumes).
