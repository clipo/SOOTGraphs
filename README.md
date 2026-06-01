# SOOTGraphs

A Shiny web application that aggregates Binghamton University SOOT (Student
Opinion of Teaching) data across an instructor's courses and terms and turns it
into summary graphs, downloadable spreadsheets, and a print-ready report.

Live app: https://lipolab.shinyapps.io/SOOTGraphs/

The original script was written by Professor Xingye Qiao of the Department of
Mathematics and adapted for the cloud by Professors Nancy Um and Carl Lipo of the
Harpur College Dean's Office, Binghamton University.

## Getting your SOOT data

Download your SOOT results from the SOOT surveys page in MyBinghamton, under the
Academic Services tab. For each course, use the download function to save its
results file. Do not rename the files: the app reads the course and term from each
filename.

The app reads two export formats and you can mix them:

- The older **CSV** export: one file per course, already tallied into answer
  counts and percentages.
- The newer **XLSX** export: one file per course, with individual student
  responses. XLSX files also carry enrollment and response-rate information that
  the CSV export does not.

## Uploading

Open the app and use the sidebar. You can upload your files two ways:

1. Select all of your individual `.csv` and `.xlsx` files at once, or
2. Put all of them into a single `.zip` file and upload that one zip.

Then click **Process uploaded data**. Files are processed in your browser session
and are not stored on the server. Empty or unreadable files are skipped with a
notice, and the rest are processed.

## What the app produces

Results are organized into four tabs:

- **Overview**: a summary score for each of the nine instructor questions,
  reported as a top-two-box percentage (the share of rating students who answered
  High or Very High). Two versions are shown side by side. The *student-weighted*
  version pools every student response, so larger courses have more influence; it
  reflects the average student's experience. The *course-weighted* version
  averages each course equally regardless of size; it reflects the typical course.
  This tab also shows the overall stacked distribution by question.
- **By Term**: the stacked distribution by term and the summary-score trend across
  terms (in both weightings), ordered chronologically.
- **By Course**: the stacked distribution by course and a question-by-course
  top-two-box heatmap, which shows whether a weaker dimension is specific to one
  course or consistent across courses.
- **Reliability**: the number of respondents per course (so small-sample
  percentages can be judged), the response rate per course (available for
  XLSX-sourced courses), and the downloadable course inventory.

The existing stacked charts exclude "Not Applicable" from the rating bars and
report its share separately. Every chart can be downloaded as a PNG and its
underlying data as a CSV.

## Print-ready report

The **Download full report (PDF)** button produces a multi-page landscape PDF: a
Binghamton-branded cover page (instructor, courses, terms, and date) followed by
every chart on its own page. It is formatted for printing or archiving.

## Running locally

Requires R with `shiny`, `tidyverse`, `ggplot2`, `viridisLite`, `bslib`,
`readxl`, and `png`. From the project directory:

```r
shiny::runApp(".")
```

The data and report logic live in `R/helpers.R`, which `app.R` sources. The UI and
server are in `app.R`. The official Binghamton logo used in the header and the PDF
cover is `www/BU-logo.png`.

## Tests

Unit tests for the data helpers (ingestion, top-two-box and distribution
computations, term ordering, upload expansion, and report generation) live in
`tests/testthat/`. Run them with:

```bash
cd tests && Rscript testthat.R
```

Fixtures in `tests/testthat/fixtures/` are aggregate, PII-free sample files.

## Deployment

The app is deployed to shinyapps.io under the `lipolab` account. A redeploy bundles
only the runtime files:

```r
rsconnect::deployApp(
  appFiles = c("app.R", "R/helpers.R", "www/BU-logo.png"),
  appName = "SOOTGraphs", account = "lipolab"
)
```

Sample data is intentionally excluded from both git and the deploy bundle because
the XLSX sample files contain instructor and student identifiers.
