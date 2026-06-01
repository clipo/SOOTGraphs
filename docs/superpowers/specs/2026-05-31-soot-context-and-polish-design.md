# SOOT Context Questions + Polish — Design Spec

Date: 2026-05-31
Status: Approved for planning
Project: SOOTGraphs (Harpur College SOOT Aggregator Shiny app)

## Purpose

Four improvements, sequenced in two phases. Phase 1 is small, low-risk polish.
Phase 2 surfaces the course/self-assessment questions the app currently discards,
which is the only item adding genuinely new insight.

The instructor pipeline (canonical schema, existing charts, top-two-box logic) is
NOT modified. Phase 2 adds a parallel "context" pipeline so there is no regression
risk to the existing analysis.

---

## Phase 1: polish

### 1a. Post-upload summary
After processing, show a compact summary above the result tabs: number of files
processed, number skipped, number of courses, number of terms, and approximate
total respondents. Rendered with `bslib::value_box`es (or a single `bslib::card`
with an inline summary) populated by a `renderUI`/`renderText` output that is empty
until processing completes.

Counts come from the instructor canonical data already built in the server:
- files processed = `nrow(work)` minus skipped; files skipped = entries that
  returned NULL from `read_soot_file`.
- courses = `length(unique(course))`; terms = `length(unique(term))`.
- approximate respondents = sum over each course/term of the maximum non-"Not
  Applicable" response count across questions (the same basis as the existing
  course inventory).

### 1b. Multi-instructor warning
After processing, compute `instructors <- unique(na.omit(full$instructor))`. If more
than one distinct instructor is present, show a dismissible warning notification
listing them and noting that results combine multiple instructors. CSV files carry
no instructor, so this fires only when XLSX uploads span instructors.

### 1c. Cleanup and CI
- Delete `app2.R` (stale predecessor; not referenced anywhere, never deployed).
- Remove `library(data.table)` and `library(purrr)` from `app.R` after confirming
  by grep that neither is used.
- Add a favicon by reusing the bundled logo:
  `tags$head(tags$link(rel = "icon", href = "BU-logo.png"))`. Kills the 404; no new
  asset.
- Add `.github/workflows/tests.yml`: on push/PR, set up R, install the runtime +
  test packages, and run `cd tests && Rscript testthat.R`, failing the job on test
  failure.

---

## Phase 2: course context questions

### Scope
A curated, evaluative subset of the discarded questions:
- Interest before vs. after the course
- Difficulty and workload (relative to other courses)
- Expected grade
- Usefulness of course materials (texts, homework, labs, examinations, discussions)

Excluded as descriptive-only: Year in School, requirements fulfilled, expected
grade vs. GPA, and free-text comments.

### Data model and XLSX decoding
The instructor schema's `ANS_TEXT` is an ordered factor over the six instructor
levels; the context questions use different vocabularies, so they live in a
separate table with `ANS_TEXT` as plain character.

Define a constant `CONTEXT_QUESTIONS` in `R/helpers.R`: a list, one entry per
curated question, each with the canonical CSV question text, an ordered label
vector for charting, a group key, and the XLSX code-to-label map. The maps below
were derived from the sample data and validated against the CSV label
vocabularies:

| Question (canonical text) | Group | Labels (order) | XLSX codes |
|---|---|---|---|
| My interest in subject before course | interest | Low, Medium, High | 1=Low,2=Medium,3=High |
| My interest in subject after course | interest | Low, Medium, High | 1=Low,2=Medium,3=High |
| Difficulty (relative to other courses) | demands | Low, Medium, High | 1,2,3 |
| Workload (relative to other courses) | demands | Low, Medium, High | 1,2,3 |
| Usefulness of texts | usefulness | Low, Medium, High | 0=Not Applicable,1=Low,2=Medium,3=High |
| Usefulness of homework assignments | usefulness | Low, Medium, High | 0=NA,1,2,3 |
| Usefulness of lab assignments | usefulness | Low, Medium, High | 0=NA,1,2,3 |
| Usefulness of examinations | usefulness | Low, Medium, High | 0=NA,1,2,3 |
| Usefulness of class discussions | usefulness | Low, Medium, High | 0=NA,1,2,3 |
| Expected Grade | grade | A, B, C, D, F, P, NP, Don't Know | 1=A,2=B,3=C,4=D,5=F,6=P,7=NP,8=Don't Know |

Note: the Expected Grade code-to-letter map is the least-certain inference; it is
encoded explicitly here and should be re-verified if Binghamton changes the export.
The XLSX question is matched to its canonical text via the existing
`strip_question_prefix` against the QuestionMapper.

### Readers
New helpers in `R/helpers.R`, returning a long table
`(course, term, QUES_TEXT, ANS_TEXT, count)` for the curated questions only:
- `read_context_csv(path, name)`: filter the CSV to the curated questions; keep
  `ANS_TEXT` and `ANS_COUNT` (renamed `count`) directly (labels already present).
- `read_context_xlsx(path, name)`: read RawData + QuestionMapper; for each curated
  question column, map each non-blank numeric code to its label via
  `CONTEXT_QUESTIONS`; drop blanks; aggregate to counts.
- `read_context_file(path, name)`: dispatch on extension; unsupported -> NULL.

The server builds `context_full` by running the same `expand_uploads` work list
through `read_context_file` (a second pass; files are few and small).

### Charts (new "Course Context" tab, a 5th tab)
Each chart pools counts across uploaded courses (student-weighted), gets a PNG +
CSV download, and joins the PDF report. New plot helpers in `R/helpers.R`:
- `plot_interest_shift(context)`: two stacked bars, Before and After, composed of
  Low/Medium/High; subtitle reports the change in percent "High".
- `plot_course_demands(context)`: two stacked bars, Difficulty and Workload
  (Low/Medium/High).
- `plot_expected_grade(context)`: bar of the expected-grade distribution
  (A…Don't Know) as percentages.
- `plot_material_usefulness(context)`: stacked Low/Medium/High bars per material
  (texts, homework, labs, examinations, discussions), with "Not Applicable"
  excluded from the composition (consistent with the instructor charts).

Charts degrade gracefully: if the uploaded files contain none of a chart's
questions (e.g., older CSVs missing an item, or all responses blank), the helper
returns a labeled empty-state placeholder rather than erroring.

### Report
The four context charts are appended to the `build_report_pdf` plot list in the
server, so the PDF report includes them after the instructor charts.

### Test fixture
Extend `tests/testthat/fixtures/make_xlsx_fixture.R` to add two context questions
(interest before, interest after) as Question 4 and Question 5 with their mapper
text and numeric codes, and regenerate `TEST101-01_FALL25.xlsx`. The existing
instructor tests are unaffected because the instructor reader filters to the nine
instructor questions, which do not include these. (Hand-computed expected: with a
small fixed set of codes, the decoded label counts are deterministic.)

---

## Edge cases
- Older CSVs missing a curated question: that question simply contributes no rows;
  the corresponding chart shows its empty-state placeholder.
- XLSX blank responses: dropped (not counted), same as the instructor path.
- An out-of-range or unmapped XLSX code: dropped with the row skipped (defensive),
  so a coding change cannot mislabel data silently.
- Mixed CSV + XLSX: both contribute to `context_full` through the shared work list.
- Multi-instructor: handled by the Phase 1 warning; context charts still pool.

## Testing
- Phase 1: a unit test for the summary-stat computation (a small helper
  `upload_summary(full, work_nrow)` returning the counts) so the numbers are
  verified, not just rendered. Multi-instructor detection tested via a small helper
  `distinct_instructors(full)`.
- Phase 2: `read_context_csv` tested against a real CSV fixture (curated questions
  present, correct labels, correct counts for one question). `read_context_xlsx`
  tested against the extended synthetic fixture (codes decode to the right labels
  and counts). Each context plot helper rendered against fixture-derived data to a
  temp PNG with no error, plus the empty-state branch.
- Full suite stays green; the existing instructor and report tests are unchanged.

## Deployment
No new runtime dependency (bslib value_box is bslib, already bundled; favicon
reuses the logo). The deploy bundle is unchanged: `app.R`, `R/helpers.R`,
`www/BU-logo.png`. `app2.R` removal also removes it from any future bundle.

## Out of scope
- Year in School, requirements fulfilled, expected-grade-vs-GPA, and free-text
  comments (comments are sensitive and would need a separate, opt-in design).
- Per-course or per-instructor breakdowns of the context questions (pooled only).
- Any change to the instructor canonical schema or its charts.
