# SOOT Binghamton Redesign + Flexible Upload + PDF Report — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebrand the SOOT app with Binghamton identity in a bslib sidebar + tabbed layout, accept either individual CSV/XLSX files or a single ZIP, and add a one-click print-ready multi-page PDF report of all charts.

**Architecture:** Two new tested helpers in `R/helpers.R` (`expand_uploads`, `build_report_pdf`). The server swaps its ingestion loop to run uploads through `expand_uploads` first, and gains a `downloadReport` handler. The entire `ui` object is replaced with a `bslib::page_fluid` branded header + `layout_sidebar` + `navset_card_tab`. All existing server output IDs and chart logic are unchanged.

**Tech Stack:** R, Shiny, bslib (Bootstrap 5 theming; already installed), ggplot2, grid + png (PDF cover/logo), readxl, testthat, pdftools (tests only).

**Builds on:** Phases A and B (merged to main). All chart objects already exist in the server: `overall_plot`, `term_plot`, `course_plot`, `summary_student_plot`, `summary_course_plot`, `trend_student_plot`, `trend_course_plot`, `response_count_plot`, `heatmap_plot`, `response_rate_plot`. `instructor_related_ques` carries `course`, `term`, `instructor`.

---

## File structure

- Modify `R/helpers.R` — add `expand_uploads()` and `build_report_pdf()`.
- Create `www/BU-logo.png` — the official Binghamton logotype (committed asset).
- Modify `app.R` — server ingestion uses `expand_uploads`; add `downloadReport` handlers; replace the `ui` object with the branded bslib layout.
- Modify `tests/testthat/test-helpers.R` — tests for the two new helpers.
- Modify `README.md` — note the redesign, zip upload, and PDF report.

---

## Task C1: expand_uploads()

**Files:** Modify `R/helpers.R`, `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-helpers.R`:

```r
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
```

If `withr` is not installed, replace `withr::with_dir(d, expr)` with `{ old <- setwd(d); on.exit(setwd(old)); expr }` in each test. Check first: `Rscript -e 'cat(requireNamespace("withr", quietly=TRUE))'`.

- [ ] **Step 2: Run to verify failure**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — `could not find function "expand_uploads"`.

- [ ] **Step 3: Implement in `R/helpers.R`**

Append:

```r
# Expand an upload batch into a work list of individual SOOT files.
# Any .zip is extracted (recursively) and its .csv/.xlsx members added; other
# files pass through. macOS archive cruft (__MACOSX/, ._ files) is ignored.
# Returns a tibble(path, name) where name is the basename used downstream.
expand_uploads <- function(paths, names) {
  out_path <- character(0)
  out_name <- character(0)
  for (i in seq_along(paths)) {
    if (grepl("\\.zip$", names[i], ignore.case = TRUE)) {
      exdir <- tempfile("unzip_"); dir.create(exdir)
      tryCatch(utils::unzip(paths[i], exdir = exdir), error = function(e) NULL)
      inner <- list.files(exdir, pattern = "\\.(csv|xlsx)$", ignore.case = TRUE,
                          recursive = TRUE, full.names = TRUE)
      inner <- inner[!grepl("(^|/)(__MACOSX/|\\._)", inner)]
      for (f in inner) { out_path <- c(out_path, f); out_name <- c(out_name, basename(f)) }
    } else {
      out_path <- c(out_path, paths[i]); out_name <- c(out_name, names[i])
    }
  }
  tibble::tibble(path = out_path, name = out_name)
}
```

- [ ] **Step 4: Run to verify pass**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add expand_uploads to accept individual files or a zip bundle"
```

---

## Task C2: build_report_pdf()

**Files:** Modify `R/helpers.R`, `tests/testthat/test-helpers.R`

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-helpers.R`:

```r
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
    expect_equal(pdftools::pdf_info(f)$pages, 3)  # cover + 2 charts
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
```

- [ ] **Step 2: Run to verify failure**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: FAIL — `could not find function "build_report_pdf"`.

- [ ] **Step 3: Implement in `R/helpers.R`**

Add `library(grid)` near the top library() calls. Append:

```r
# Build a print-ready landscape PDF: a branded cover page then one page per chart.
# plots: named list of ggplot objects (names become page captions).
# meta:  list(courses, terms, instructor, n_courses, date); NULL/empty -> placeholder.
# logo_path: optional PNG embedded on the cover when present.
build_report_pdf <- function(file, plots, meta, logo_path = "www/BU-logo.png") {
  grDevices::pdf(file, paper = "USr", width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off())

  if (length(plots) == 0 || is.null(meta)) {
    grid::grid.newpage()
    grid::grid.text("No data processed yet.\nUpload SOOT files and click 'Process uploaded data' first.",
                    gp = grid::gpar(fontsize = 18))
    return(invisible(file))
  }

  # Cover page
  grid::grid.newpage()
  if (!is.null(logo_path) && file.exists(logo_path)) {
    img <- png::readPNG(logo_path)
    grid::grid.raster(img, x = 0.5, y = 0.82, height = grid::unit(0.9, "inches"))
  }
  grid::grid.text("Harpur College SOOT Aggregation Report", y = 0.66,
                  gp = grid::gpar(fontsize = 26, fontface = "bold", col = "#005A43"))
  instr <- meta$instructor
  instr <- if (is.null(instr) || all(is.na(instr))) NULL else paste(unique(stats::na.omit(instr)), collapse = ", ")
  lines <- c(
    if (!is.null(instr)) paste("Instructor:", instr),
    paste("Courses:", paste(sort(unique(meta$courses)), collapse = ", ")),
    paste("Terms:", paste(meta$terms, collapse = ", ")),
    paste("Generated:", format(meta$date, "%B %d, %Y"))
  )
  grid::grid.text(paste(lines, collapse = "\n"), y = 0.42,
                  gp = grid::gpar(fontsize = 13, col = "#5A5C5B"))
  grid::grid.text("Generated by the Harpur College SOOT Aggregator", y = 0.06,
                  gp = grid::gpar(fontsize = 9, col = "#9C9D9D"))

  # One page per chart
  for (nm in names(plots)) {
    print(plots[[nm]] +
            ggplot2::labs(caption = "Generated by the Harpur College SOOT Aggregator") +
            ggplot2::theme(plot.caption = ggplot2::element_text(color = "#9C9D9D", size = 8)))
  }
  invisible(file)
}
```

- [ ] **Step 4: Run to verify pass**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: PASS (3-page and 1-page assertions hold).

- [ ] **Step 5: Commit**

```bash
git add R/helpers.R tests/testthat/test-helpers.R
git commit -m "Add build_report_pdf: branded multi-page print-ready report"
```

---

## Task C3: Bundle the official BU logo

**Files:** Create `www/BU-logo.png`

- [ ] **Step 1: Download the official logo into www/**

Run:

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs
mkdir -p www
curl -fsSL -o www/BU-logo.png "https://www.binghamton.edu/communications-and-marketing/img/logos/BU-logo.png"
file www/BU-logo.png
Rscript -e 'img <- png::readPNG("www/BU-logo.png"); cat("logo dims:", paste(dim(img), collapse="x"), "\n")'
```
Expected: `file` reports PNG image data; the dims line prints a height x width x channels triple. If the download fails (non-PNG or 0 bytes), STOP and report BLOCKED — do not commit a broken asset.

- [ ] **Step 2: Commit**

```bash
git add www/BU-logo.png
git commit -m "Add official Binghamton University logotype asset"
```

---

## Task C4: Wire expand_uploads into the server ingestion

**Files:** Modify `app.R` (the ingestion loop in `observeEvent`)

- [ ] **Step 1: Replace the ingestion loop**

In `app.R`, the current loop reads:

```r
        full <- NULL
        for (i in seq_along(input$csvs$datapath)) {
            one <- tryCatch(
                read_soot_file(input$csvs$datapath[i], input$csvs$name[i]),
                error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
            )
            if (!is.null(one)) full <- bind_rows(full, one)
        }
```

Replace it with:

```r
        # Expand the upload batch (any .zip is extracted) into a work list, then
        # read each file through the ingestion layer into the canonical schema.
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

- [ ] **Step 2: Verify the app parses and a zip ingests end-to-end**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e 'invisible(parse("app.R")); cat("parses\n")'
Rscript -e '
  suppressMessages(source("R/helpers.R"))
  # build a zip from two fixtures, expand it, read through the pipeline
  d <- tempfile("z_"); dir.create(d)
  file.copy("tests/testthat/fixtures/ANTH243-01_FALL24.csv", file.path(d, "ANTH243-01_FALL24.csv"))
  file.copy("tests/testthat/fixtures/TEST101-01_FALL25.xlsx", file.path(d, "TEST101-01_FALL25.xlsx"))
  z <- tempfile(fileext=".zip"); old<-setwd(d); utils::zip(z, list.files("."), flags="-q"); setwd(old)
  work <- expand_uploads(z, "bundle.zip")
  full <- do.call(dplyr::bind_rows, lapply(seq_len(nrow(work)), function(i) read_soot_file(work$path[i], work$name[i])))
  cat("courses from zip:", paste(sort(unique(full$course)), collapse=","), "\n")
'
```
Expected: `parses`; courses from zip include `ANTH243` and `TEST101`.

- [ ] **Step 3: Commit**

```bash
git add app.R
git commit -m "Run uploads through expand_uploads so a zip bundle is accepted"
```

---

## Task C5: Branded bslib UI (header + sidebar + tabs) and PDF report wiring

**Files:** Modify `app.R` (replace the `ui` object; add report handlers in the server)

- [ ] **Step 1: Add `library(bslib)`**

In `app.R`, add `library(bslib)` after `library(viridisLite)` (line 6 area).

- [ ] **Step 2: Replace the entire `ui <- fluidPage(...)` block**

Replace everything from `ui <- fluidPage(` through its closing `)` (the line before `server <- function(input, output) {`) with this exact block:

```r
ui <- page_fluid(
    title = "Harpur College SOOT Aggregator",
    theme = bs_theme(version = 5, primary = "#005A43", success = "#6CC24A"),
    tags$head(tags$style(HTML("
      .bu-header { background:#ffffff; display:flex; align-items:center; gap:18px; padding:14px 22px; }
      .bu-header img { height:54px; }
      .bu-header .bu-title { color:#005A43; font-weight:700; font-size:1.6rem; margin:0; }
      .bu-accent { height:6px; background:#005A43; margin-bottom:14px; }
    "))),
    div(class = "bu-header",
        tags$img(src = "BU-logo.png", alt = "Binghamton University"),
        tags$h1(class = "bu-title", "Harpur College SOOT Aggregator")),
    div(class = "bu-accent"),
    layout_sidebar(
        sidebar = sidebar(
            width = 360,
            title = "Upload and process",
            p("Upload your SOOT results to generate aggregated graphs. The app accepts two
               formats: the older CSV export (one file per course, already tallied) and the
               newer XLSX export (one file per course, individual responses). You can mix both."),
            p("Two ways to upload: select all your individual .csv or .xlsx files at once, or
               put them together in a single .zip file and upload that one zip. Do not rename
               the files. Then click Process uploaded data."),
            fileInput("csvs", label = "SOOT files (.csv, .xlsx, or one .zip)",
                      multiple = TRUE, accept = c(".csv", ".xlsx", ".zip")),
            actionButton("process", "Process uploaded data", class = "btn-primary"),
            downloadButton("downloadReport", "Download full report (PDF)", class = "btn-success"),
            p(class = "text-muted", style = "font-size:0.85em;",
              "Files are processed in your browser session and are not stored on the server.")
        ),
        navset_card_tab(
            id = "results",
            nav_panel(
                "Overview",
                card(card_header("Summary Score by Question"),
                     plotOutput("summary_student_plot"),
                     div(downloadButton("downloadSummaryStudentPlot", "Plot"),
                         downloadButton("downloadSummaryStudentData", "Data")),
                     plotOutput("summary_course_plot"),
                     div(downloadButton("downloadSummaryCoursePlot", "Plot"),
                         downloadButton("downloadSummaryCourseData", "Data")),
                     p("Two summaries of the same ratings are shown. The student-weighted version
                        pools every student response together, so courses with more respondents have
                        more influence; it reflects the experience of the average student. The
                        course-weighted version summarizes each course on its own and then averages
                        those course summaries equally, so every course counts the same regardless of
                        size; it reflects the typical course. The two differ when class sizes vary: a
                        single large course pulls the student-weighted number toward its own ratings,
                        while the course-weighted number gives a small seminar the same say as a large
                        lecture.")),
                card(card_header("Overall Results Aggregated by Question"),
                     plotOutput("overall_plot"),
                     div(downloadButton("downloadOverallPlot", "Plot"),
                         downloadButton("downloadOverallData", "Data")))
            ),
            nav_panel(
                "By Term",
                card(card_header("Results Aggregated by Term"),
                     plotOutput("term_plot"),
                     div(downloadButton("downloadTermPlot", "Plot"),
                         downloadButton("downloadTermData", "Data"))),
                card(card_header("Summary Score Trends by Term"),
                     plotOutput("trend_student_plot", height = "600px"),
                     div(downloadButton("downloadTrendStudentPlot", "Plot"),
                         downloadButton("downloadTrendStudentData", "Data")),
                     plotOutput("trend_course_plot", height = "600px"),
                     div(downloadButton("downloadTrendCoursePlot", "Plot"),
                         downloadButton("downloadTrendCourseData", "Data")))
            ),
            nav_panel(
                "By Course",
                card(card_header("Results Aggregated by Course"),
                     plotOutput("course_plot"),
                     div(downloadButton("downloadCoursePlot", "Plot"),
                         downloadButton("downloadCourseData", "Data"))),
                card(card_header("Question-by-Course Comparison"),
                     p("Top-two-box percentage for each question in each course. Use it to see
                        whether a weaker dimension is specific to one course or consistent across
                        courses."),
                     plotOutput("heatmap_plot", height = "500px"),
                     div(downloadButton("downloadHeatmapPlot", "Plot"),
                         downloadButton("downloadHeatmapData", "Data")))
            ),
            nav_panel(
                "Reliability",
                card(card_header("Response Counts by Course"),
                     p("Number of rating respondents per course. Percentages from courses with few
                        respondents are less reliable; use this to judge how much weight to give each
                        bar."),
                     plotOutput("response_count_plot"),
                     div(downloadButton("downloadResponseCountPlot", "Plot"),
                         downloadButton("downloadResponseCountData", "Data"))),
                card(card_header("Response Rate by Course"),
                     p("Percentage of enrolled students who responded, available for XLSX-format
                        uploads (the older CSV export does not include enrollment)."),
                     plotOutput("response_rate_plot"),
                     div(downloadButton("downloadResponseRatePlot", "Plot"),
                         downloadButton("downloadResponseRateData", "Data"))),
                card(card_header("Course Inventory"),
                     p("A course inventory with the number of responses for each course. You may add
                        a column for the overall enrollment to show the response rate for each course."),
                     div(downloadButton("downloadInventory", "Course Inventory CSV"),
                         downloadButton("downloadAnswers", "Answers CSV")))
            )
        )
    )
)
```

- [ ] **Step 3: Add a default (no-data) report handler at the top of the server function**

Immediately inside `server <- function(input, output) {`, BEFORE `observeEvent(input$process, {`, add:

```r
    # Default report handler: returns a one-page "no data" PDF if the user clicks
    # download before processing. Reassigned with the real report after processing.
    output$downloadReport <- downloadHandler(
        filename = "SOOT_Report.pdf",
        content = function(file) { build_report_pdf(file, list(), NULL) }
    )
```

- [ ] **Step 4: Add the real report handler inside observeEvent, after all plots are built**

Inside `observeEvent`, immediately BEFORE the `#Create a course inventory` line, add:

```r
        output$downloadReport <- downloadHandler(
            filename = "SOOT_Report.pdf",
            content = function(file) {
                report_plots <- list(
                    "Summary Score by Question (Student-Weighted)" = summary_student_plot,
                    "Summary Score by Question (Course-Weighted)" = summary_course_plot,
                    "Overall Results Aggregated by Question" = overall_plot,
                    "Results Aggregated by Term" = term_plot,
                    "Summary Trends by Term (Student-Weighted)" = trend_student_plot,
                    "Summary Trends by Term (Course-Weighted)" = trend_course_plot,
                    "Results Aggregated by Course" = course_plot,
                    "Question-by-Course Comparison" = heatmap_plot,
                    "Response Counts by Course" = response_count_plot,
                    "Response Rate by Course" = response_rate_plot
                )
                report_meta <- list(
                    courses = sort(unique(instructor_related_ques$course)),
                    terms = levels(order_terms(unique(instructor_related_ques$term))),
                    instructor = instructor_related_ques$instructor,
                    n_courses = length(unique(instructor_related_ques$course)),
                    date = Sys.Date()
                )
                build_report_pdf(file, report_plots, report_meta)
            }
        )
```

- [ ] **Step 5: Verify parse, app boot, and report generation**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs && Rscript -e 'invisible(parse("app.R")); cat("parses\n")'
Rscript -e 'shiny::runApp(".", port=7864, launch.browser=FALSE)' >/tmp/soot_c_app.log 2>&1 &
APP_PID=$!; sleep 8
curl -s -o /dev/null -w "HTTP %{http_code}\n" http://127.0.0.1:7864/ || echo "curl failed"
curl -s -o /dev/null -w "logo HTTP %{http_code}\n" http://127.0.0.1:7864/BU-logo.png || echo "logo failed"
kill $APP_PID 2>/dev/null; grep -i "error\|listening" /tmp/soot_c_app.log | head
```
Expected: `parses`; HTTP 200 for the app; logo HTTP 200 (www/ served); log shows "Listening on", no R error.

- [ ] **Step 6: Commit**

```bash
git add app.R
git commit -m "Rebrand UI with bslib sidebar+tabs; add PDF report download"
```

---

## Task C6: Visual verification, end-to-end, README

**Files:** Modify `README.md`

- [ ] **Step 1: Unit suite green**

Run: `cd /Users/clipo/PycharmProjects/SOOTgraphs/tests && Rscript testthat.R`
Expected: 0 failures.

- [ ] **Step 2: Visual check with the Playwright browser tool**

Launch the app (`Rscript -e 'shiny::runApp(".", port=7865, launch.browser=FALSE)'` in the background), then with the Playwright MCP browser tools: navigate to `http://127.0.0.1:7865/`, take a screenshot of the landing page (confirm the green header band, BU logo, sidebar with upload control + the two buttons, and the four result tabs render). Confirm the logo image loads (not a broken-image icon). Report what you observed; if the logo is broken or the layout is obviously wrong, STOP and report BLOCKED. Stop the app afterward.

- [ ] **Step 3: Full report end-to-end against the sample set, including a zip**

```bash
cd /Users/clipo/PycharmProjects/SOOTgraphs
rm -rf /tmp/soot_c && mkdir /tmp/soot_c && unzip -q sample-data/sample-data.zip -d /tmp/soot_c
# make a single zip of all sample files to exercise the zip path
( cd /tmp/soot_c/SOOTs && zip -q /tmp/soot_c/bundle.zip *.csv *.xlsx )
Rscript -e '
  suppressMessages({library(ggplot2); source("R/helpers.R")})
  work <- expand_uploads("/tmp/soot_c/bundle.zip", "bundle.zip")
  cat("files from bundle:", nrow(work), "\n")
  full <- NULL
  for (i in seq_len(nrow(work))) { one <- tryCatch(read_soot_file(work$path[i], work$name[i]), error=function(e) NULL); if(!is.null(one)) full <- dplyr::bind_rows(full, one) }
  irq <- dplyr::filter(full, QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
  plots <- list(
    "Summary (Student)" = plot_summary_by_question(compute_top_two_box(irq,"QUES_TEXT","student"),"Student-Weighted","n"),
    "Heatmap" = plot_question_course_heatmap(irq),
    "Response Rate" = plot_response_rate(irq))
  meta <- list(courses=sort(unique(irq$course)), terms=levels(order_terms(unique(irq$term))),
               instructor=irq$instructor, n_courses=length(unique(irq$course)), date=Sys.Date())
  f <- tempfile(fileext=".pdf"); build_report_pdf(f, plots, meta, logo_path="www/BU-logo.png")
  cat("report pages:", pdftools::pdf_info(f)$pages, " size:", file.info(f)$size, "\n")
  cat("E2E ZIP + REPORT OK\n")
'
```
Expected: files from bundle > 20; report pages == 4 (cover + 3); "E2E ZIP + REPORT OK". If anything errors, STOP and report BLOCKED.

- [ ] **Step 4: Update README.md**

Add a concise note (no em dashes): the app has a Binghamton-branded interface; uploads may be individual CSV/XLSX files or a single ZIP containing them; and a "Download full report (PDF)" button produces a print-ready multi-page report of all charts. Keep existing content.

- [ ] **Step 5: Commit**

```bash
git add README.md
git commit -m "Document branded UI, zip upload, and PDF report in README"
```

---

## Self-review notes

- **Spec coverage:** bslib theme + brand colors (C5), BU-logo header (C3, C5), sidebar + four-tab layout with the specified chart-to-tab mapping (C5), flexible upload via `expand_uploads` (C1, C4), rewritten instructions (C5 sidebar copy), printable PDF report via `build_report_pdf` with cover + per-chart pages and no-data branch (C2, C5), edge cases (zip nesting/cruft, mixed batch in C1; no-data report in C2), tests (C1, C2), visual + e2e verification (C6), deploy asset note (C3; deployment handled at merge — `appFiles` must include `www/BU-logo.png`).
- **Deployment reminder (post-merge):** the redeploy must bundle `www/BU-logo.png` in addition to `app.R` and `R/helpers.R`; `png` is pulled in by `build_report_pdf` and snapshotted automatically; `pdftools` is test-only and not referenced by app code.
- **Type consistency:** `expand_uploads(paths, names)` returns `tibble(path, name)`; the server uses `work$path`/`work$name`. `build_report_pdf(file, plots, meta, logo_path)` takes a named list of ggplots and a meta list with `courses/terms/instructor/n_courses/date`; the server builds exactly that. New names introduced: `expand_uploads`, `build_report_pdf`, output id `downloadReport`, asset `www/BU-logo.png`. All existing output IDs are reused unchanged in the new UI.
- **No chart logic changes:** the ggplot builders and compute helpers are untouched; only UI containers and the ingestion front-door change.
