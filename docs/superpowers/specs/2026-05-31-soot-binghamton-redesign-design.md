# SOOT App Binghamton Redesign + Flexible Upload — Design Spec

Date: 2026-05-31
Status: Approved for planning
Project: SOOTGraphs (Harpur College SOOT Aggregator Shiny app)

## Purpose

The app works but looks unstyled and scrolls through eleven result sections in one
column. This work rebrands it with Binghamton University identity, reorganizes the
results into a sidebar + tabbed layout, and makes uploads flexible: users may
select individual CSV/XLSX files OR a single ZIP containing them. The instructions
are rewritten to explain the two file formats and the two upload paths.

It also adds a one-click print-ready PDF report of all charts. No analysis or
chart computation changes: this is UI, layout, branding, upload ingestion, and
report assembly only.

## Brand reference

From Binghamton University brand guidelines:
- Primary deep green `#005A43` (dominant color).
- Accents: yellow-green `#CEDC00`, light green `#6CC24A`.
- Gray `#5A5C5B`, black `#000000`, white `#FFFFFF`.
- Official logotype `BU-logo.png`: must not be recreated, altered, or distorted;
  requires clear space; units may not create independent marks. Source image:
  `https://www.binghamton.edu/communications-and-marketing/img/logos/BU-logo.png`.

## Theming

Replace the bare `fluidPage` with a **bslib** Bootstrap 5 theme (`bslib::bs_theme`).
bslib ships as a Shiny dependency, so nothing new to deploy. Theme variables:
- `primary = "#005A43"`, `success = "#6CC24A"`.
- Base font: a clean sans-serif (system sans stack or a bundled Google sans via
  `bslib::font_google` if it loads cleanly; system sans is the safe default).
- Charts are unchanged; their viridis palette coexists with the green theme.

## Header

A page header (`bslib::card` or a styled `div`) with a **white background band**
containing:
- The official BU logo on the left, bundled at `www/BU-logo.png`, displayed
  unaltered at a readable height (about 48-60 px) with clear space around it.
- The text title "Harpur College SOOT Aggregator" in brand green.
- A thin green (`#005A43`) accent bar beneath the band.

Using the logo on white avoids any recoloring of the mark.

## Layout: sidebar + tabbed results

Use `bslib::page_sidebar` (or `page_fluid` + `layout_sidebar`).

**Sidebar** (stays in view): rewritten instructions, the `fileInput`, the
`actionButton("process", ...)`, and the "Download full report (PDF)" button.

**Main panel**: `bslib::navset_card_tab` with four tabs; each chart wrapped in a
card with its existing download buttons. Chart-to-tab assignment:

- **Overview**: Summary score by question (student-weighted) + (course-weighted) +
  the two-weightings explanatory paragraph; Overall Results Aggregated by Question
  (stacked).
- **By Term**: Results Aggregated by Term (stacked); Summary Trends by Term
  (student-weighted) + (course-weighted).
- **By Course**: Results Aggregated by Course (stacked); Question-by-Course
  heatmap.
- **Reliability**: Response Counts by Course; Response Rate by Course; Course
  Inventory downloads.

All existing `plotOutput`, `downloadButton`, and server output IDs are preserved;
only their container/placement in the UI changes. The server logic is unchanged
except for the upload-expansion step below.

## Flexible upload + ingestion

`fileInput("csvs", ..., multiple = TRUE, accept = c(".csv", ".xlsx", ".zip"))`.
The input id `csvs` is kept so existing server references work.

New tested helper in `R/helpers.R`:

```
expand_uploads(paths, names) -> tibble(path, name)
```

For each uploaded file:
- If its name ends in `.zip` (case-insensitive): extract to a fresh temp dir and
  add every contained `.csv`/`.xlsx` file (recursively; nested folders handled).
  Use the basename of each extracted file as its `name` so `parse_course_term`
  still works.
- Otherwise: pass through as `(path, name)`.

The server replaces the current per-upload loop with: build the work list via
`expand_uploads(input$csvs$datapath, input$csvs$name)`, then read each entry
through the existing `read_soot_file`. Empty/unsupported/zero-instructor entries
are skipped with warnings exactly as today. A zip containing no usable files
yields the existing "No usable data found" notification.

## Instructions copy (sidebar)

> Upload your SOOT results to generate aggregated graphs. The app accepts two
> formats: the older CSV export (one file per course, already tallied) and the
> newer XLSX export (one file per course, individual responses). You can mix both.
>
> Two ways to upload: select all your individual .csv or .xlsx files at once, or
> put them together in a single .zip file and upload that one zip. Do not rename
> the files. Then click Process uploaded data.
>
> Files are processed in your browser session and are not stored on the server.

## Printable PDF report

A "Download full report (PDF)" button in the sidebar produces a single,
print-ready, multi-page landscape PDF of everything, built with R's built-in
`pdf()` device (no LaTeX, no headless browser; robust on shinyapps.io).

New tested helper in `R/helpers.R`:

```
build_report_pdf(file, plots, meta)
```

- `plots`: a named, ordered list of the ggplot objects the server already builds
  (overall, by-term, by-course stacked; summary student/course; trends
  student/course; response counts; heatmap; response rate), each paired with a
  caption.
- `meta`: list(courses, terms, instructor, n_courses, date) for the cover.
- Behavior: open `pdf(file, paper = "USr", width = 11, height = 8.5)` (landscape
  US letter), draw a **cover page** (BU logo via `png::readPNG` + `grid::grid.raster`,
  the title "Harpur College SOOT Aggregation Report", instructor name when
  available from XLSX metadata, the list of courses and terms included, and the
  generation date from `Sys.Date()`), then print each chart on its own page with a
  caption header and a footer line ("Generated by the Harpur College SOOT
  Aggregator"). `dev.off()` to finalize.

Server: a `downloadHandler("downloadReport", filename = "SOOT_Report.pdf")` collects
the already-built plot objects and the meta and calls `build_report_pdf`. If the
button is clicked before any data is processed, the handler guards on the absence
of data and writes a one-page "No data processed yet" PDF.

Dependency note: `png` is used to embed the logo (commonly installed; confirm in
the plan and add to the dependency set if missing). No LaTeX or Chromium.

## Edge cases

- A `.zip` plus loose files in one upload batch: both handled (zip expanded, loose
  files passed through).
- Nested directories inside the zip: recursive listing picks them up.
- A zip with no CSV/XLSX, or only empty files: skipped with warnings, then the
  "No usable data found" notification.
- Non-CSV/XLSX/zip files: skipped with a warning (existing dispatch behavior).
- macOS zip cruft (`__MACOSX/`, `.DS_Store`): ignored because only `.csv`/`.xlsx`
  extensions are collected.

## Testing

- `expand_uploads` unit tests: a zip built on the fly from existing fixtures
  (returns the contained files), a mixed loose-file + zip batch, a nested-folder
  zip, and a zip with macOS cruft (cruft ignored).
- The full ingestion still passes the existing mixed CSV+XLSX and sample-set
  checks.
- `build_report_pdf` test: render to a temp PDF from fixture-derived plots and
  meta, assert the file exists and is non-empty, and (if `pdftools` is available)
  assert the page count equals cover + number of charts; the no-data branch
  produces a one-page PDF.
- Visual verification: launch the app locally and use the Playwright browser tool
  to screenshot the branded header, sidebar, and each tab to confirm the redesign
  renders. (Charts themselves are already covered by Phase A/B tests.)

## Deployment note

The bundled `www/BU-logo.png` must be included in the rsconnect deploy
(`appFiles` must add `www/BU-logo.png` alongside `app.R` and `R/helpers.R`).
bslib is already an installed dependency. `png` (for embedding the logo in the
PDF cover) and, optionally, `pdftools` (tests only) must be confirmed installed in
the plan and declared if missing. No LaTeX or Chromium toolchain is introduced.

## Out of scope

- Any change to chart computations, weightings, or the canonical schema.
- Custom or reversed logo variants (logo used unaltered on white).
- Licensed brand fonts (a clean sans is sufficient).
